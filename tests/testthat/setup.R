dbToTest <- Sys.getenv("DB_TO_TEST", "duckdb CDMConnector")
collectCohort <- function(cohort, id) {
  x <- cohort |>
    dplyr::filter(.data$cohort_definition_id == .env$id) |>
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") |>
    dplyr::collect() |>
    dplyr::arrange(subject_id, cohort_start_date, cohort_end_date)
  attr(x, "cohort_set") <- NULL
  attr(x, "cohort_attrition") <- NULL
  attr(x, "cohort_codelist") <- NULL
  class(x) <- c("tbl_df", "tbl", "data.frame")
  return(x)
}
compareCohort <- function(cohort1, id1, cohort2, id2) {
  if (!identical(collectCohort(cohort1, id1), collectCohort(cohort2, id2))) {
    cli::cli_abort("cohorts are not equal")
  }
  return(invisible(TRUE))
}
copyCdm <- function(cdm) {
  # create the source to copy the cdm to
  prefix <- "coco_test_"
  to <- switch(
    dbToTest,
    "duckdb CDMConnector" = CDMConnector::dbSource(
      con = duckdb::dbConnect(drv = duckdb::duckdb(dbdir = ":memory:")),
      writeSchema = c(schema = "main", prefix = prefix)
    ),
    "sql server CDMConnector" = NULL,
    "redshift CDMConnector" = NULL,
    "postgres CDMConnector" = NULL,
    "local omopgenerics" = omopgenerics::newLocalSource()
  )

  # insert cdm to my source of interest
  cdm <- omopgenerics::insertCdmTo(cdm = cdm, to = to)

  return(cdm)
}
dropCreatedTables <- function(cdm) {
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::everything())
}
countDuckdbTempTables <- function(con){
  DBI::dbGetQuery(con, "SHOW ALL TABLES") |>
    dplyr::filter(database == "temp") |>
    dplyr::tally() |>
    dplyr::pull("n")
}
countDuckdbPermanentTables <- function(con){
  DBI::dbGetQuery(con, "SHOW ALL TABLES") |>
    dplyr::filter(database != "temp") |>
    dplyr::tally() |>
    dplyr::pull("n")
}
testIndexes <- TRUE
