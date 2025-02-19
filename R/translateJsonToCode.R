
translateJsonToCode <- function(jsonPath,
                                codePath) {
  # initial checks
  omopgenerics::assertCharacter(jsonPath, length = 1)
  omopgenerics::assertCharacter(codePath, length = 1)
  jsons <- list.files(path = jsonPath) |>
    purrr::keep(\(x) tools::file_ext(x) == "json")
  if (length(jsons) == 0) {
    cli::cli_warn(c("x" = "No json file found in {.path {jsonPath}}."))
    return(invisible(character()))
  }
  if (!dir.exists(codePath)) {
    cli::cli_warn(c("!" = "Creating {.path {codePath}} as it did not exist."))
    dir.create(codePath)
  }

  # extract information
  codelists <- omopgenerics::emptyCodelist()
  code <- character()
  for (k in seq_along(jsons)) {
    # read json file
    json <- jsonlite::read_json(jsons[k])

    # get codelists
    codelists <- c(codelists, readCodelists(json))

    # get cohort creation code
    code <- c(code, codeFromJson(json))
  }

  # export codelists
  codelistsDir <- file.path(codePath, "codelists")
  if (dir.exists(codelistsDir)) {
    cli::cli_warn(c("!" = "Overwritting {.path {codelistsDir}} folder."))
    unlink(codelistsDir, recursive = TRUE)
  }
  dir.create(codelistsDir)
  omopgenerics::exportCodelist(x = codelists, path = codelistsDir, type = "csv")

  # export code
  pathCode <- file.path(codePath, "instantiateCohorts.R")
  if (file.exists(pathCode)) {
    cli::cli_warn(c("!" = "Overwritting {.path {codePath}} file."))
    unlink(pathCode)
  }
  code <- headerCohortCreation(code)
  writeLines(text = code, con = pathCode)

  return(invisible(code))
}
readCodelists <- function(json) {

}
codeFromJson <- function(json) {

}
headerCohortCreation <- function(code) {
  c(
    "# This code assumes that there is an object `cdm` that is a cdm_reference object.",
    "",
    "# import codelists ----",
    "codelists <- omopgenerics::importCodelists(here::here('codelists')) # modify this paths if you move the codelist folder",
    "",
    "# code to create cohorts ----",
    code
  )
}
