
#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
newCohortDefinition <- function(x) {
  omopgenerics::assertList(x, named = TRUE)
  x <- constructCohortDefinition(x)
  validateCohortDefinition(x)
}

#' Title
#'
#' @param path
#' @param recursive
#'
#' @returns
#' @export
#'
#' @examples
importCohortDefinition <- function(path, recursive = FALSE) {
  # files to import
  files <- findFiles(path = path, recursive = recursive)

  # read content as json
  files |>
    purrr::imap(\(x, nm) {
      content <- jsonlite::read_json(path = x, pretty = TRUE)
      tryCatch({

      }, error = function(e) {
        cli::cli_inform(c("!" = "", as.character(e)))
      })
    }) |>
    purrr::compact() |>
    omopgenerics::bind()
}

#' Title
#'
#' @param x
#' @param path
#'
#' @returns
#' @export
#'
#' @examples
exportCohortDefinition <- function(x, path) {

}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
cohortDefinitionFromCode <- function(x) {

}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
codeFromCohortDefinition <- function(x) {

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
  fun <- switch(type, codelist = newCodelist, codelist_with_details = newCodelistWithDetails,
                concept_set_expression = newConceptSetExpression)
  x <- purrr::compact(purrr::imap(x, function(element, nm) {
    if (!inherits(element, "cohort_definition")) {
      element <- tryCatch(newCohortDefinition(element), error = function(e) NULL)
      if (is.null(element)) {
        cli::cli_inform(c(`!` = "Element `{nm}` eliminated as could not be converted to {.cls cohort_definition}."))
      }
      else {
        cli::cli_inform(c(i = "Element `{nm}` converted to {.cls cohort_definition}."))
      }
    }
    element
  }))

  unlist(x, recursive = FALSE) |>
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
validateCohortDefinition <- function(x) {
  problems <- purrr::imap_chr(x, \(x, nm) {

  })
  if (length(problems) > 0) {
    cli::cli_abort(c(x = "Cohort definition not well formatted", problems))
  }
  invisible(x)
}
findFiles <- function(path, recursive, call = parent.frame()) {
  assertCharacter(path, call = call)
  assertLogical(recursive, length = 1, call = call)
  path <- as.character(unlist(purrr::map(path, function(x) {
    if (!stringr::str_starts(string = x, pattern = "https")) {
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
