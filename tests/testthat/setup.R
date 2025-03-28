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
writeSchema <- function(dbToTest = Sys.getenv("DB_TO_TEST", "duckdb")) {
  prefix <- paste0("coco_", sample(letters, 4) |> paste0(collapse = ""), "_")
  switch(dbToTest,
    "duckdb" = c(schema = "main", prefix = prefix),
    "sql server" = c(catalog = "ohdsi", schema = "dbo", prefix = prefix),
    "redshift" = c(schema = "resultsv281", prefix = prefix)
  )
}
connection <- function(dbToTest = Sys.getenv("DB_TO_TEST", "duckdb")) {
  switch(dbToTest,
    "duckdb" = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    "sql server" = DBI::dbConnect(
      odbc::odbc(),
      Driver = "ODBC Driver 18 for SQL Server",
      Server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
      Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
      UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
      PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
      TrustServerCertificate = "yes",
      Port = 1433
    ),
    "redshift" = DBI::dbConnect(
      RPostgres::Redshift(),
      dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
      port = Sys.getenv("CDM5_REDSHIFT_PORT"),
      host = Sys.getenv("CDM5_REDSHIFT_HOST"),
      user = Sys.getenv("CDM5_REDSHIFT_USER"),
      password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
    )
  )
}
copyCdm <- function(cdm) {
  CDMConnector::copyCdmTo(
    con = connection(), cdm = cdm, schema = writeSchema(), overwrite = TRUE
  )
}
countDuckdbTempTables <- function(con){
  duckdb_temp_tables <- DBI::dbGetQuery(con, "SHOW ALL TABLES")
  duckdb_temp_tables |>
    dplyr::filter(database == "temp") |>
    dplyr::tally() |>
    dplyr::pull("n")
}
countDuckdbPermanentTables <- function(con){
  duckdb_temp_tables <- DBI::dbGetQuery(con, "SHOW ALL TABLES")
  duckdb_temp_tables |>
    dplyr::filter(database != "temp") |>
    dplyr::tally() |>
    dplyr::pull("n")
}
testIndexes <- TRUE
