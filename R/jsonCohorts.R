
#' Create cohort from an 'ATLAS' defined json file.
#'
#' @param cdm A cdm_reference object.
#' @param jsonPath Path to json files.
#' @param name Name of the cohort to create.
#'
#' @return A cdm_reference object with the cohort created. # DO we want to output cohort
#' @export
#'
jsonCohort <- function(cdm,
                       jsonPath,
                       name) {
  rlang::check_installed("jsonlite")
  rlang::check_installed("styler")
  # initial checks
  omopgenerics::assertCharacter(jsonPath, length = 1)
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  name <- omopgenerics::validateNameArgument(name = name)

  # read json files
  jsons <- readJsons(jsonPath = jsonPath)

  # read codelists
  codelists <- readCodelists(json = jsons, cdm = cdm)
  dictionary <- attr(codelists, "dictionary")

  # cohort constructor definition
  definition <- createCohortConstructorDefinition(jsons, dictionary)

  # create code from json
  code <- codeFromDefinition(definition, name)

  # print code
  cli::cli_inform(c("i" = "Code to generate cohorts:"))
  print(code)

  # eval code
  #rlang::eval_tidy(rlang::parse_exprs(code))

  return(cdm)
}

#' Extract codelists from a set of json files.
#'
#' @param jsonPath A path to a folder with json files.
#' @param cdm A cdm_reference object.
#'
#' @return A `codelist` object.
#' @export
#'
codelistFromJson <- function(jsonPath,
                             cdm) {

}

#' Extract the code to generate an `ATLAS` json cohort.
#'
#' @param jsonPath A path to a folder with json files.
#' @param cdm A cdm_reference object.
#'
#' @return A character vector with the code to create the json equivalent cohorts.
#' @export
#'
codeFromJson <- function(jsonPath,
                         cdm) {

}

readJsons <- function(jsonPath) {
  jsons <- list.files(path = jsonPath, full.names = TRUE) |>
    purrr::keep(\(x) tools::file_ext(x) == "json") |>
    purrr::set_names(\(x) basename(tools::file_path_sans_ext(x)))
  if (length(jsons) == 0) {
    cli::cli_warn(c("x" = "No json file found in {.path {jsonPath}}."))
  } else {
    c("i" = "The following cohorts were identified:",
      paste0("{.pkg ", names(jsons), "}: ", jsons)) |>
      cli::cli_inform()
  }
  purrr::map(jsons, jsonlite::read_json)
}
readCodelists <- function(jsons, cdm) {
  # extract concepts from json file
  concepts <- purrr::imap_dfc(jsons, \(x, nm) {
    purrr::map_df(x$ConceptSets, \(xx) {
      purrr::map_df(xx$expression$items, \(xxx) {
        dplyr::tibble(
          concept_id = xxx$concept$CONCEPT_ID,
          exclude = xxx$isExcluded %||% FALSE,
          descendants = xxx$includeDescendants %||% FALSE
        )
      }) |>
        dplyr::mutate(name = xx$name, id = xx$id)
    }) |>
      dplyr::mutate(json = nm)
  }) |>
    dplyr::relocate("json", "id", "name")

  # get descendants
  descendants <- concepts |>
    dplyr::filter(.data$descendants) |>
    dplyr::distinct(.data$concept_id)
  if (nrow(descendants) > 0) {
    nm <- omopgenerics::uniqueTableName()
    cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = descendants)
    descendants <- cdm[[nm]] |>
      dplyr::inner_join(
        cdm$concept_ancestor |>
          dplyr::select(
            "concept_id" = "ancestor_concept_id", "descendant_concept_id"
          ),
        by = "concept_id"
      ) |>
      dplyr::collect() |>
      dplyr::mutate(descendants = TRUE)
    omopgenerics::dropSourceTable(cdm = cdm, name = nm)
    concepts <- concepts |>
      dplyr::left_join(
        descendants,
        by = c("concept_id", "descendants"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(concept_id = dplyr::coalesce(
        .data$descendant_concept_id, .data$concept_id
      )) |>
      dplyr::select(!c("descendant_concept_id", "descendants"))
  } else {
    concepts <- concepts |>
      dplyr::select(!"descendants")
  }

  # exclude codes
  concepts <- concepts |>
    dplyr::select(!"exclude") |>
    dplyr::anti_join(
      concepts |>
        dplyr::filter(.data$exclude) |>
        dplyr::select(!"exclude"),
      by = c("json", "id", "name", "concept_id")
    )

  # create dictionary
  dictionary <- concepts |>
    dplyr::distinct(.data$json, .data$id, .data$name)

  # check repeated codelists
  repeatedNames <- concepts |>
    dplyr::group_by(.data$name) |>
    dplyr::group_split() |>
    as.list() |>
    purrr::keep(\(x) {
      ngroups <- nrow(dplyr::distinct(x, .data$json, .data$id))
      if (ngroups == 1) {
        return(FALSE)
      }
      groups <- x |>
        dplyr::group_by(.data$json, .data$id) |>
        dplyr::group_split() |>
        as.list() |>
        purrr::map(\(x) sort(unique(x$concept_id)))
      # whether codelists are the same
      length(unique(groups)) != 1
    }) |>
    purrr::map_chr(\(x) unique(x$name))
  if (length(repeatedNames) > 0) {
    c(x = "There are multiple codelist with the same name, but diference concepts: {.pkg {repeatedNames}}.") |>
      cli::cli_abort()
  }

  # create codelists
  concepts <- concepts |>
    dplyr::group_by(.data$name) |>
    dplyr::group_split() |>
    as.list()
  names(concepts) <- purrr::map_chr(concepts, \(x) unique(x$name))
  concepts <- concepts |>
    purrr::map(\(x) as.integer(unique(x$concept_id))) |>
    omopgenerics::newCodelist()

  attr(concepts, "dictionary") <- dictionary

  return(concepts)
}
createCohortConstructorDefinition <- function(json, dictionary) {
  purrr::imap(json, \(x, nm) {
    # get concept set
    ids <- unlist(x$PrimaryCriteria$CriteriaList)
    ids <- unique(ids[grepl("CodesetId", names(ids))])
    nms <- dictionary$name[dictionary$json == nm & dictionary$id %in% ids]
    if (length(nms) > 1) {
      conceptSet <- paste0(
        'list("', nm, '" = c(',
        paste0('codelists[["', nms, '"]]', collapse = ', '),
        '))'
      )
    } else {
      conceptSet <- paste0('list("', nm, '" = codelists[["', nms, '"]])')
    }

    # prior observation
    priorObs <- x$PrimaryCriteria$ObservationWindow$PriorDays
    if (priorObs > 0) {
      requirePriorObservation <- list(minPriorObservation = priorObs)
    } else {
      requirePriorObservation <- NULL
    }

    # future observation
    futureObs <- x$PrimaryCriteria$ObservationWindow$PostDays
    if (futureObs > 0) {
      requireFutureObservation <- list(minFutureObservation = futureObs)
    } else {
      requireFutureObservation <- NULL
    }

    # is first entry
    if (x$PrimaryCriteria$PrimaryCriteriaLimit$Type == "First") {
      requireIsFirstEntry <- list()
    } else {
      requireIsFirstEntry <- NULL
    }

    list(
      conceptCohort = list(
        conceptSet = conceptSet,
        exit = '"event_start_date"',
        overlap = '"merge"',
        useSourceFiles = "FALSE"
      ),
      requireIsFirstEntry = requireIsFirstEntry,
      requirePriorObservation = requirePriorObservation,
      requireFutureObservation = requireFutureObservation
    )
  })
}
codeFromDefinition <- function(definition, name) {
  # TODO optimise instantiating multiple cohorts at the same time

  # if multiple cohorts they will be generated separately and binded later
  if (length(definition) > 1) {
    nms <- paste0(name, seq_along(definition))
  } else {
    nms <- name
  }
  nms <- rlang::set_names(as.list(nms), names(definition))

  # generate the code
  code <- purrr::imap(definition, \(x, nm) {
    x <- purrr::keep(x, \(x) !is.null(x))
    # original cohort
    if ("conceptCohort" %in% names(x)) {
      code <- paste0(
        "cdm$", nms[[nm]], " <- CohortConstructor::conceptCohort(\ncdm = cdm,\n",
        collapseArguments(x$conceptCohort),
        "\n)"
      )
    } else if ("measurementCohort" %in% names(x)) {
      # TODO
    }
    # require functions
    nms <- names(x)[!names(x) %in% c("conceptCohort", "measurementCohort")]
    for (nm in nms) {
      code <- paste0(
        code, "|>\nCohortConstructor::", nm, "(\n", collapseArguments(x[[nm]]),
        "\n)"
      )
    }
    return(code)
  }) |>
    paste0(collapse = "\n\n")

  # do we need to bind
  if (length(definition) > 1) {
    code <- paste0(
      code, "\n\nomopgenerics::bind(", paste0("cdm$", nms, collapse = ", "),
      ", name = '", name, "')\n\nomopgenerics::dropSourceTable(c('",
      paste0(nms, collapse = "', '"), "'))"
    )
  }

  # style code
  styler::style_text(code)
}
collapseArguments <- function(x) {
  if (length(x) == 0) {
    return("")
  }
  paste0(names(x), " = ", x, collapse = ",\n")
}
