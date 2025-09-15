
test_that("test adds indexes", {
  skip_on_cran()

  cdm <- omock::mockCdmFromTables(tables = list(my_cohort = dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2009-01-01"),
    cohort_end_date = as.Date("2009-01-02")
  ))) |>
    copyCdm()

  if (dbToTest == "postgres CDMConnector") {
    # get connection
    con <- CDMConnector::cdmCon(cdm = cdm)

    # check indexes at start
    indexes_start <- dplyr::tbl(con, "pg_indexes") |>
      dplyr::filter(.data$table_name == "coco_test_my_cohort") |>
      dplyr::pull("indexname")
    expect_true(length(indexes_start) == 0)

    # add indexes
    expect_no_error(cdm$my_cohort <- addCohortTableIndex(cdm$my_cohort))

    # check that one index exists
    indexes_after <- dplyr::tbl(con, "pg_indexes") |>
      dplyr::filter(.data$table_name == "coco_test_my_cohort") |>
      dplyr::pull("indexname")
    expect_true(length(indexes_after) == 1)

    # add indexes again
    expect_no_error(cdm$my_cohort <- addCohortTableIndex(cdm$my_cohort))

    # check indexes remain the same
    indexes_after <- dplyr::tbl(con, "pg_indexes") |>
      dplyr::filter(.data$table_name == "coco_test_my_cohort") |>
      dplyr::pull("indexname")
    expect_true(length(indexes_after) == 1)
  } else {
    # do nothing for a cohort
    expect_no_error(addCohortTableIndex(cdm$my_cohort))

    # throw error if not a cohort
    expect_error(addCohortTableIndex(cdm$person))
  }

  dropCreatedTables(cdm = cdm)
})
