
#' Instantiate a cohort from a cohort definition
#'
#' `instantiateCohortDefinition()` evaluates a stored cohort-definition
#' pipeline against an OMOP CDM and returns the resulting cohort table.
#'
#' @inheritParams cohortDefinitionDoc
#' @inheritParams cdmDoc
#' @inheritParams nameDoc
#' @inheritParams conceptSetDoc
#'
#' @return A cohort table containing the instantiated cohort.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' definition <- cohortDefinitionFromCode(
#'   'cdm$cohort <- conceptCohort(cdm = cdm, name = "cohort", '
#'   'conceptSet = codelist["condition"])'
#' )
#' instantiateCohortDefinition(
#'   cohortDefinition = definition,
#'   cdm = cdm,
#'   name = "cohort",
#'   conceptSet = list(condition = 201826L)
#' )
#' }
instantiateCohortDefinition <- function(cohortDefinition,
                                        cdm,
                                        name,
                                        conceptSet = NULL) {
  # input validation
  cohortDefinition <- validateCohortDefinition(cohortDefinition)
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  name <- omopgenerics::validateNameArgument(name = name, cdm = cdm)

  if (length(cohortDefinition) == 0) {
    cli::cli_abort("`cohortDefinition` does not contain any definitions.")
  }

  # check codelists
  conceptSet <- conceptSet |>
    omopgenerics::validateConceptSetArgument(cdm = cdm)
  neededConcepts <- cohortDefinition$needed_codelists
  notPresent <- neededConcepts[!neededConcepts %in% names(conceptSet)]
  if (length(notPresent) > 0) {
    cli::cli_abort(c(x = "Codelists: {.var {notPresent}} must be provided in `conceptSet`."))
  }

  # get the code
  code <- codeFromSingleCohortDefinition(
    x = cohortDefinition,
    targetName = name
  )

  # evaluate the code
  cdm[[name]] <- rlang::eval_tidy(
    rlang::parse_expr(code),
    data = list(cdm = cdm, codelist = conceptSet)
  )

  return(cdm[[name]])
}

#' Create a cohort definition
#'
#' @description
#' `newCohortDefinition()` validates and constructs a collection of cohort
#' definitions that can be exported, converted to code, or instantiated.
#'
#' @param x A named list of cohort definitions. Each definition must contain a
#'   `name` and a `definition` list of function calls.
#'
#' @return A `cohort_definition` object.
#' @export
#'
#' @examples
#' \donttest{
#' newCohortDefinition(list(
#'   cohort = list(
#'     name = "cohort",
#'     definition = list(
#'       list(
#'         package = "CohortConstructor",
#'         fun = "conceptCohort",
#'         parameters = list(conceptSet = "condition")
#'       )
#'     )
#'   )
#' ))
#' }
newCohortDefinition <- function(x) {
  omopgenerics::assertList(x, named = TRUE)
  x <- constructCohortDefinition(x)
  validateCohortDefinition(x)
}

#' Import cohort definitions from JSON
#'
#' @description
#' `importCohortDefinition()` reads one or more JSON cohort-definition files.
#' A directory contributes all files ending in `.json`.
#'
#' @param path A path, URL, or directory containing JSON cohort definitions.
#' @param recursive If `TRUE`, search directories recursively for JSON files.
#'
#' @return A `cohort_definition` object. Invalid files are reported and
#'   skipped.
#' @export
#'
#' @examples
#' \donttest{
#' definitions <- importCohortDefinition("path/to/definitions")
#' }
importCohortDefinition <- function(path, recursive = FALSE) {
  # files to import
  files <- findFiles(path = path, recursive = recursive)
  if (length(files) == 0) {
    return(newCohortDefinition(list()))
  }

  # read content as json
  definitions <- files |>
    purrr::imap(\(x, nm) {
      tryCatch({
        content <- readCohortDefinitionJson(x)
        if (isSingleCohortDefinition(content)) {
          content <- stats::setNames(list(content), content$name)
        }
        newCohortDefinition(content)
      },
      error = function(e) {
        cli::cli_inform(c("!" = "", as.character(e)))
        return(NULL)
      })
    }) |>
    purrr::compact()

  if (length(definitions) == 0) {
    return(newCohortDefinition(list()))
  }
  do.call(omopgenerics::bind, definitions)
}

#' Export cohort definitions to JSON
#'
#' @description
#' `exportCohortDefinition()` writes one JSON file per cohort definition. The
#' output directory must already exist.
#'
#' @param x A `cohort_definition` object.
#' @param path An existing directory in which to write one JSON file per
#'   cohort definition.
#'
#' @return The input `cohort_definition`, invisibly.
#' @export
#'
#' @examples
#' \donttest{
#' exportCohortDefinition(definitions, "path/to/definitions")
#' }
exportCohortDefinition <- function(x, path) {
  x <- validateCohortDefinition(x)
  omopgenerics::assertCharacter(path, length = 1)
  if (!dir.exists(path)) {
    cli::cli_abort(c(x = "Path {.path {path}} does not exist."))
  }

  for (nm in names(x)) {
    jsonlite::write_json(
      x = x[[nm]],
      path = file.path(path, paste0(nm, ".json")),
      pretty = TRUE,
      auto_unbox = TRUE
    )
  }

  invisible(x)
}

#' Create a cohort definition from R code
#'
#' @description
#' `cohortDefinitionFromCode()` parses a cohort-construction assignment and
#' stores its pipeline as structured function calls. Only literal values and
#' named codelist references are parsed; the supplied code is not evaluated.
#'
#' @param x A single character string, expression, or call containing an
#'   assignment such as `cdm$cohort <- conceptCohort(...) |> requireSex(...)`.
#'
#' @return A `cohort_definition` object.
#' @export
#'
#' @examples
#' code <- paste0(
#'   'cdm$cohort <- conceptCohort(cdm = cdm, name = "cohort", ',
#'   'conceptSet = codelist["condition"])'
#' )
#' cohortDefinitionFromCode(code)
cohortDefinitionFromCode <- function(x) {
  expression <- parseCohortDefinitionCode(x)

  if (!rlang::is_call(expression, "<-", n = 2)) {
    cli::cli_abort("Code must contain an assignment of the form `cdm$cohort_name <- ...`.")
  }

  cohortName <- cohortNameFromAssignment(expression[[2]])
  steps <- stepsFromPipeline(expression[[3]])
  definition <- stats::setNames(
    list(list(name = cohortName, definition = steps)),
    cohortName
  )

  newCohortDefinition(definition)
}

#' Generate R code from a cohort definition
#'
#' @description
#' `codeFromCohortDefinition()` converts structured cohort definitions into
#' executable R pipeline code.
#'
#'
#' @return A character string containing one assignment per cohort definition.
#' @export
#'
#' @examples
#' code <- 'cdm$cohort <- conceptCohort(cdm = cdm, name = "cohort", conceptSet = codelist["condition"])'
#' definition <- cohortDefinitionFromCode(code)
#' codeFromCohortDefinition(definition)
codeFromCohortDefinition <- function(x) {
  x <- validateCohortDefinition(x)
  purrr::map_chr(x, codeFromSingleCohortDefinition) |>
    paste(collapse = "\n\n")
}

#' @export
print.cohort_definition <- function(x, ...) {
  cli::cli_h1("{length(x)} cohort definition{?s}")
  cli::cat_line("")
  disp <- 6
  len <- min(length(x), disp)
  for (i in seq_len(len)) {
    cli::cat_line(paste0("- ", names(x)[i]))
  }
  if (length(x) > disp) {
    cli::cat_line(paste0("along with ", length(x) - disp, " more definitions"))
  }
  invisible(x)
}

#' @export
#' @importFrom omopgenerics bind
bind.cohort_definition <- function(...) {
  c(...)
}

#' @export
c.cohort_definition <- function(...) {
  list(...) |>
    purrr::imap(\(element, nm) {
      if (!inherits(element, "cohort_definition")) {
        element <- tryCatch(newCohortDefinition(element), error = function(e) NULL)
        if (is.null(element)) {
          cli::cli_inform(c(`!` = "Element `{nm}` eliminated as could not be converted to {.cls cohort_definition}."))
        } else {
          cli::cli_inform(c(i = "Element `{nm}` converted to {.cls cohort_definition}."))
        }
      }
      element
    }) |>
    purrr::compact() |>
    unlist(recursive = FALSE) |>
    removeRepeated() |>
    renameCohortDefinitions() |>
    newCohortDefinition()
}

#' @export
`[.cohort_definition` <- function(x, i) {
  cl <- class(x)
  obj <- NextMethod()
  class(obj) <- cl
  return(obj)
}

constructCohortDefinition <- function(x) {
  structure(.Data = x, class = c("cohort_definition"))
}
removeRepeated <- function(x) {
  nms <- names(x)
  if (length(nms) != length(unique(nms))) {
    eliminate <- rep(FALSE, length(nms))
    for (k in seq_along(nms)) {
      if (!eliminate[k]) {
        id <- which(nms[k] == nms)
        id <- id[id > k]
        cont <- x[[k]]
        if (length(id) > 0) {
          for (i in id) {
            if (identical(cont, x[[i]])) {
              eliminate[i] <- TRUE
            }
          }
        }
      }
    }
    x <- x[!eliminate]
  }
  return(x)
}
renameCohortDefinitions <- function(x) {
  nms <- names(x)
  if (length(nms) != length(unique(nms))) {
    duplicated <- names(which(table(nms) > 1))
    id <- nms %in% duplicated
    dup <- nms[id]
    nameChange <- character()
    for (k in seq_along(dup)) {
      oldName <- dup[k]
      newName <- purrr::map_chr(oldName, \(x) findNewName(x, nms))
      nms <- c(nms, newName)
      nameChange <- c(nameChange, rlang::set_names(newName, oldName))
    }
    msg <- purrr::imap_chr(nameChange, \(x, nm) paste0(nm, " -> ", x))
    names(msg) <- rep("*", length(msg))
    c("!" = "Repeated names found between cohorts, renamed as:", msg) |>
      cli::cli_inform()
    names(x)[id] <- unname(nameChange)
  }
  x
}
findNewName <- function (name, usedNames) {
  usedNames <- usedNames[startsWith(x = usedNames, prefix = paste0(name, "_"))]
  k <- 1
  newName <- paste0(name, "_", k)
  while (newName %in% usedNames) {
    k <- k + 1
    newName <- paste0(name, "_", k)
  }
  return(newName)
}
validateCohortDefinition <- function(x, call = parent.frame()) {
  omopgenerics::assertClass(x, "cohort_definition", call = call)

  problems <- character()

  # validate
  for (nm in names(x)) {
    xk <- x[[nm]]

    # fields
    notPresent <- c("name", "definition") |>
      purrr::keep(\(field) !field %in% names(xk))
    if (length(notPresent) > 0) {
      problems <- c(
        problems,
        paste0("fields not present in `", nm,"`: ", paste0(notPresent, collapse = "; "), ".")
      )
    }

    # populated needed fields
    if (!"needed_codelists" %in% names(xk)) {
      xk$needed_codelists <- neededCodelists(xk)
    }
    if (!"needed_cohorts" %in% names(xk)) {
      xk$needed_cohorts <- neededCohorts(xk)
    }

    x[[nm]] <- xk
  }

  if (length(problems) > 0) {
    cli::cli_abort(c(x = "Cohort definition not well formatted", problems), call = call)
  }

  invisible(x)
}
findFiles <- function(path, recursive, call = parent.frame()) {
  omopgenerics::assertCharacter(path, call = call)
  omopgenerics::assertLogical(recursive, length = 1, call = call)
  path <- as.character(unlist(purrr::map(path, function(x) {
    isUrl <- grepl("^https://", x, ignore.case = TRUE)
    if (!isUrl) {
      if (!file.exists(x)) {
        cli::cli_warn(c(x = "directory {.path {x}} does not exist"))
        return(NULL)
      }
      if (file.info(x)$isdir) {
        x <- list.files(path = x, full.names = TRUE, pattern = "\\.json$", recursive = recursive)
      }
    }
    return(x)
  })))
  pathClean <- sub("\\?.*$", "", path)
  names(path) <- tools::file_path_sans_ext(basename(pathClean))
  as.list(path)
}
readCohortDefinitionJson <- function(path) {
  isUrl <- grepl("^https://", path, ignore.case = TRUE)
  if (isUrl) {
    destination <- tempfile(fileext = ".json")
    on.exit(unlink(destination), add = TRUE)
    utils::download.file(path, destination, mode = "wb", quiet = TRUE)
    path <- destination
  }
  jsonlite::read_json(path = path, pretty = TRUE) |>
    decodeJsonSpecialValues()
}

isSingleCohortDefinition <- function(x) {
  is.list(x) &&
    all(c("name", "definition") %in% names(x)) &&
    is.character(x$name) &&
    length(x$name) == 1
}

decodeJsonSpecialValues <- function(x) {
  if (is.list(x)) {
    if (is.null(names(x)) && length(x) == 0) {
      return(character())
    }
    out <- purrr::map(x, decodeJsonSpecialValues)
    if (is.null(names(x)) && length(out) > 0 &&
        all(purrr::map_lgl(out, \(element) {
          is.atomic(element) && length(element) == 1
        }))) {
      return(unlist(out, use.names = FALSE))
    }
    return(out)
  }

  if (is.character(x) && length(x) == 1 && x %in% c("Inf", "-Inf", "NaN")) {
    return(as.numeric(x))
  }

  x
}

parseCohortDefinitionCode <- function(x, call = parent.frame()) {
  if (is.character(x)) {
    omopgenerics::assertCharacter(x, length = 1, call = call)
    return(rlang::parse_expr(x))
  }
  if (is.expression(x) && length(x) == 1) {
    return(x[[1]])
  }
  if (rlang::is_call(x)) {
    return(x)
  }
  cli::cli_abort("`x` must be a single character string, expression, or call.", call = call)
}

cohortNameFromAssignment <- function(x, call = parent.frame()) {
  if (rlang::is_call(x, "$") && identical(rlang::as_string(x[[2]]), "cdm")) {
    return(rlang::as_string(x[[3]]))
  }
  if (rlang::is_call(x, "[[") && identical(rlang::as_string(x[[2]]), "cdm") &&
      is.character(x[[3]]) && length(x[[3]]) == 1) {
    return(x[[3]])
  }
  cli::cli_abort("The assignment target must be `cdm$cohort_name` or `cdm[[\"cohort_name\"]]`.", call = call)
}

cohortFunctionFromCall <- function(x, call = parent.frame()) {
  if (!rlang::is_call(x)) {
    cli::cli_abort("Each cohort-definition step must be a function call.", call = call)
  }

  functionCall <- x[[1]]
  if (rlang::is_call(functionCall, "::") || rlang::is_call(functionCall, ":::")) {
    package <- rlang::as_string(functionCall[[2]])
    fun <- rlang::as_string(functionCall[[3]])
  } else if (rlang::is_symbol(functionCall)) {
    package <- "CohortConstructor"
    fun <- rlang::as_string(functionCall)
  } else {
    cli::cli_abort("Cohort-definition functions must be named calls.", call = call)
  }

  list(package = package, fun = fun)
}

isNestedCohortCall <- function(x) {
  if (!rlang::is_call(x)) {
    return(FALSE)
  }
  name <- rlang::call_name(x)
  !is.null(name) && !name %in% c("[", "[[", "$", "c", "list", "-", "+", "*", "/", "::", ":::")
}

stepsFromPipeline <- function(x, call = parent.frame()) {
  info <- cohortFunctionFromCall(x, call = call)
  args <- rlang::call_args(x)
  argNames <- names(args)

  nested <- which(purrr::map_lgl(args, isNestedCohortCall))
  if (length(nested) > 1) {
    cli::cli_abort("Each pipeline step can have only one input cohort.", call = call)
  }

  if (length(nested) == 1) {
    nestedId <- nested[[1]]
    previous <- stepsFromPipeline(args[[nestedId]], call = call)
    args <- args[-nestedId]
    argNames <- argNames[-nestedId]
    current <- cohortDefinitionStepFromCall(
      x = x,
      info = info,
      args = args,
      argNames = argNames,
      root = FALSE,
      call = call
    )
    return(c(previous, list(current)))
  }

  list(cohortDefinitionStepFromCall(
    x = x,
    info = info,
    args = args,
    argNames = argNames,
    root = TRUE,
    call = call
  ))
}

cohortDefinitionStepFromCall <- function(x, info, args, argNames, root, call = parent.frame()) {
  if (anyDuplicated(argNames[argNames != ""])) {
    cli::cli_abort("Each cohort-definition function argument must have a unique name.", call = call)
  }

  if (any(argNames == "")) {
    formalNames <- tryCatch(
      names(formals(utils::getExportedValue(info$package, info$fun))),
      error = function(e) character()
    )
    unnamed <- which(argNames == "")
    if (length(formalNames) < max(unnamed, 0)) {
      cli::cli_abort("All function arguments must be named or belong to a known exported function.", call = call)
    }
    argNames[unnamed] <- formalNames[unnamed]
  }
  names(args) <- argNames

  if (root) {
    if ("cdm" %in% names(args)) {
      if (!rlang::is_symbol(args$cdm, "cdm")) {
        cli::cli_abort("The `cdm` argument must be the `cdm` object.", call = call)
      }
      args$cdm <- NULL
    }
    if ("name" %in% names(args)) {
      if (!is.character(args$name) || length(args$name) != 1) {
        cli::cli_abort("The root `name` argument must be a single string.", call = call)
      }
      args$name <- NULL
    }
  } else {
    args$cohort <- NULL
  }

  parameters <- purrr::map(args, codeValueToRValue)
  names(parameters) <- names(args)
  list(
    package = info$package,
    fun = info$fun,
    parameters = parameters
  )
}

codeValueToRValue <- function(x, call = parent.frame()) {
  if (rlang::is_null(x) || is.atomic(x)) {
    return(x)
  }
  if (rlang::is_symbol(x)) {
    value <- rlang::as_string(x)
    if (value %in% c("Inf", "NA", "NA_real_", "NA_integer_", "NA_character_", "NaN")) {
      if (value == "Inf") return(Inf)
      if (value == "NaN") return(NaN)
      if (value == "NA_real_") return(NA_real_)
      if (value == "NA_integer_") return(NA_integer_)
      if (value == "NA_character_") return(NA_character_)
      return(NA)
    }
    cli::cli_abort("Only literal values and codelist references can be parsed; `{value}` is not supported.", call = call)
  }
  if (!rlang::is_call(x)) {
    cli::cli_abort("Unsupported parameter expression.", call = call)
  }

  functionName <- rlang::call_name(x)
  args <- rlang::call_args(x)
  if (functionName %in% c("-", "+") && length(args) == 1 &&
      is.numeric(args[[1]]) && length(args[[1]]) == 1 && is.infinite(args[[1]])) {
    return(if (functionName == "-") -Inf else Inf)
  }
  if (functionName %in% c("[", "[[") && length(args) == 2 &&
      rlang::is_symbol(args[[1]], "codelist")) {
    reference <- codeValueToRValue(args[[2]], call = call)
    if (!is.character(reference) || length(reference) != 1) {
      cli::cli_abort("Codelist references must use a single character name.", call = call)
    }
    return(reference)
  }
  if (functionName == "c") {
    return(do.call(c, purrr::map(args, codeValueToRValue, call = call)))
  }
  if (functionName == "list") {
    values <- purrr::map(args, codeValueToRValue, call = call)
    names(values) <- names(args)
    return(values)
  }

  cli::cli_abort("Unsupported parameter expression `{functionName}`.", call = call)
}

codeFromSingleCohortDefinition <- function(x,
                                           targetName = x$name,
                                           call = parent.frame()) {
  if (length(x$definition) == 0) {
    cli::cli_abort("A cohort definition must contain at least one function call.", call = call)
  }

  steps <- x$definition
  root <- steps[[1]]
  rootParameters <- c(
    list(cdm = quote(cdm), name = targetName),
    root$parameters
  )
  code <- cohortDefinitionCallText(root, rootParameters)

  if (length(steps) > 1) {
    for (step in steps[-1]) {
      code <- paste0(code, " |> ", cohortDefinitionCallText(step, step$parameters))
    }
  }

  target <- if (make.names(targetName) == targetName) {
    paste0("cdm$", targetName)
  } else {
    paste0("cdm[[", encodeString(targetName, quote = "\""), "]]")
  }
  paste(target, "<-", code)
}
cohortDefinitionCallText <- function(step, parameters) {
  functionName <- if (identical(step$package, "CohortConstructor")) {
    step$fun
  } else {
    paste0(step$package, "::", step$fun)
  }
  parameterExpressions <- purrr::imap(parameters, \(value, nm) {
    if (identical(nm, "conceptSet") && is.character(value) && length(value) == 1) {
      rlang::call2("[", quote(codelist), value)
    } else {
      decodeJsonSpecialValues(value)
    }
  })
  names(parameterExpressions) <- names(parameters)
  call <- rlang::call2(functionName, !!!parameterExpressions)
  paste(deparse(call, width.cutoff = 500), collapse = " ")
}
neededCodelists <- function(x) {
  neededElements(x, c("conceptSet"))
}
neededCohorts <- function(x) {
  neededElements(x, c("targetCohortTable"))
}
neededElements <- function(x, key) {
  purrr::map(x$definition, \(def) {
    parameters <- def$parameters
    if (is.null(parameters)) {
      return(NULL)
    }
    unlist(parameters[key[key %in% names(parameters)]])
  }) |>
    unlist() |>
    as.character() |>
    unique() |>
    sort()
}
