#' Add an index to a cohort table
#'
#' @description
#' Adds an index on subject_id and cohort_start_date to a cohort table. Note,
#' currently only indexes will be added if the table is in a postgres or sql
#' server database.
#'
#' @inheritParams cohortDoc
#'
#' @return The cohort table
#' @export
#'
addCohortTableIndex <-  function(cohort) {

  cohort <- omopgenerics::validateCohortArgument(cohort)

  addIndex(cohort,
           cols = c("subject_id", "cohort_start_date"))

  cohort
}


addIndex <- function(cohort, cols) {
  cdm <- omopgenerics::cdmReference(cohort)
  name <- omopgenerics::tableName(cohort)

  tblSource <- attr(cohort, "tbl_source")
  if(is.null(tblSource)){
    return(invisible(NULL))
  }
  dbType <- attr(tblSource, "source_type")
  if(is.null(dbType)){
    return(invisible(NULL))
  }

  if (dbType == "postgresql") {
    con <- attr(cdm, "dbcon")
    schema <- attr(cdm, "write_schema")
    if(length(schema) > 1){
      prefix <- attr(cdm, "write_schema")["prefix"]
      schema <- attr(cdm, "write_schema")["schema"]
    } else {
      prefix <- NULL
    }

    existingIndex <- DBI::dbGetQuery(con,
               paste0("SELECT * FROM pg_indexes WHERE",
                      " schemaname = '",
                       schema,
                      "' AND tablename = '",
                      paste0(prefix, name),
               "';"))
    if(nrow(existingIndex) > 0){
      cli::cli_inform("Index already existing so no new index added.")
      return(invisible(NULL))
    } else {
      cli::cli_inform("Adding indexes to table")
    }

    cols <- paste0(cols, collapse = ",")

    query <- paste0(
      "CREATE INDEX ON ",
      paste0(schema, ".", prefix, name),
      " (",
      cols,
      ");"
    )
    suppressMessages(DBI::dbExecute(con, query))
  } else if (dbType == c("sql server")) {
    con <- attr(cdm, "dbcon")
    schema <- attr(cdm, "write_schema")
    if(length(schema) > 2){
      prefix <- attr(cdm, "write_schema")["prefix"]
      schema1 <- attr(cdm, "write_schema")["schema1"]
      schema2 <- attr(cdm, "write_schema")["schema2"]
    } else {
      schema1 <- attr(cdm, "write_schema")["schema1"]
      schema2 <- attr(cdm, "write_schema")["schema2"]
      prefix <- NULL
    }

    existingIndex <- DBI::dbGetQuery(con,
                                     paste0("
  SELECT i.name AS index_name,
         o.name AS table_name,
         DB_NAME() AS database_name
  FROM sys.indexes i
  JOIN sys.objects o ON o.object_id = o.object_id
  WHERE o.name = '", paste0(prefix, name) ,"'
  AND i.name IS NOT NULL

  UNION ALL

  SELECT i.name AS index_name,
         o.name AS table_name,
         'tempdb' AS database_name
  FROM tempdb.sys.indexes i
  JOIN tempdb.sys.objects o ON i.object_id = o.object_id
  WHERE o.name = '", paste0(prefix, name) ,"'
  AND i.name IS NOT NULL
"))


    if(nrow(existingIndex) > 0){
      cli::cli_inform("Index already existing so no new index added.")
      return(invisible(NULL))
    } else {
      cli::cli_inform("Adding indexes to table")
    }

    cols <- paste0(cols, collapse = ",")

    query <- paste0(
      "CREATE INDEX IX_", name, "_", gsub(",", "_", cols),
      " ON ", schema1, ".", schema2, ".", prefix, name,
      " (", cols, ");"
    )
    suppressMessages(DBI::dbExecute(con, query))
  }

  return(invisible(NULL))

}
