#' Add an index to a cohort table
#'
#' @description
#' Adds an index on subject_id and cohort_start_date to a cohort table. Note,
#' currently only indexes will be added if the table is in a postgres database.
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


addIndex <- function(cohort, cols, unique = FALSE) {
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

    if(isFALSE(unique)){
      query <- paste0(
        "CREATE INDEX ON ",
        paste0(schema, ".", prefix, name),
        " (",
        cols,
        ");"
      )
    } else {
    query <- paste0(
      "CREATE UNIQUE INDEX ON ",
      paste0(schema, ".", prefix, name),
      " (",
      cols,
      ");"
    )
    }

    suppressMessages(DBI::dbExecute(con, query))

    # lastly, update statistics
    cli::cli_inform("Update statistics")
    query <- paste0(
      "ANALYZE ",
      paste0(schema, ".", prefix, name)
    )
    suppressMessages(DBI::dbExecute(con, query))

  }

  return(invisible(NULL))

}
