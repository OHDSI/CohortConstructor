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
  x <- dplyr::as_tibble(x)
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
    "redshift CDMConnector" = CDMConnector::dbSource(
      con = RPostgres::dbConnect(
        RPostgres::Redshift(),
        dbname = stringr::str_split_1(Sys.getenv("CDM5_REDSHIFT_SERVER"), "/")[2],
        host = stringr::str_split_1(Sys.getenv("CDM5_REDSHIFT_SERVER"), "/")[1],
        user = Sys.getenv("CDM5_REDSHIFT_USER"),
        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"),
        port = 5439
      ),
      writeSchema = c(schema = "public", prefix = prefix)
    ),
    "sqlserver CDMConnector" = CDMConnector::dbSource(
      con = odbc::dbConnect(
        odbc::odbc(),
        Driver = "ODBC Driver 18 for SQL Server",
        Server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
        Database = "CDMV5",
        UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
        PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
        TrustServerCertificate="yes",
        Port = 1433
      ),
      writeSchema = c(catalog = "ohdsi", schema = "dbo", prefix = prefix)
    ),
    "postgres CDMConnector" = CDMConnector::dbSource(
      con = RPostgres::dbConnect(
        RPostgres::Postgres(),
        dbname = stringr::str_split_1(Sys.getenv("CDM5_POSTGRESQL_SERVER"), "/")[2],
        host = stringr::str_split_1(Sys.getenv("CDM5_POSTGRESQL_SERVER"), "/")[1],
        user = Sys.getenv("CDM5_POSTGRESQL_USER"),
        password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
      ),
      writeSchema = c(schema = "public", prefix = prefix)
    ),
    "snowflake CDMConnector" = CDMConnector::dbSource(
      con = odbc::dbConnect(
        odbc::odbc(),
        SERVER = stringr::str_extract(Sys.getenv("CDM_SNOWFLAKE_CONNECTION_STRING"), "(?<=//)[^?]+(?=\\?)"),
        UID = Sys.getenv("CDM_SNOWFLAKE_USER"),
        PWD = Sys.getenv("CDM_SNOWFLAKE_PASSWORD"),
        DATABASE = "ATLAS",
        WAREHOUSE = stringr::str_extract(Sys.getenv("CDM_SNOWFLAKE_CONNECTION_STRING"), "(?i)(?<=\\bwarehouse=)[^&?#]+"),
        Driver = "SnowflakeDSIIDriver"
      ),
      writeSchema = c(catalog = "ATLAS", schema = "RESULTS", prefix = prefix)
    ),
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
