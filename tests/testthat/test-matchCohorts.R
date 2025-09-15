test_that("matchCohorts runs without errors", {
  skip_on_cran()

  cdm <- omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = "cohort1", numberCohorts = 2) |>
    omock::mockCohort(name = "cohort2", numberCohorts = 2) |>
    copyCdm()

  expect_no_error(a <- matchCohorts(cohort = cdm$cohort1,
                                    name = "new_cohort",
                                    ratio = 1))

  expect_no_error(a <- matchCohorts(cohort = cdm$cohort2,
                                    name = "new_cohort"))

  expect_no_error(matchCohorts(cohort = cdm$cohort2,
                               name = "new_cohort",
                               ratio = 3))

  expect_no_error(matchCohorts(cohort = cdm$cohort2,
                               name = "new_cohort",
                               ratio = Inf))

  expect_no_error(matchCohorts(cohort = cdm$cohort2,
                               name = "new_cohort",
                               matchSex = FALSE,
                               matchYearOfBirth = TRUE))

  expect_no_error(matchCohorts(cohort = cdm$cohort2,
                               name = "new_cohort",
                               matchSex = TRUE,
                               matchYearOfBirth = FALSE))

  expect_no_error(b <- matchCohorts(cohort = cdm$cohort2,
                                    name = "new_cohort",
                                    matchSex = FALSE,
                                    matchYearOfBirth = FALSE))

  expect_no_error(a <- matchCohorts(cohort = cdm$cohort2,
                                    name = "new_cohort",
                                    cohortId = c(1,2),
                                    matchSex = TRUE,
                                    matchYearOfBirth = TRUE,
                                    ratio = 2))
  expect_true(nrow(settings(a)) == 4)

  expect_no_error(c <- matchCohorts(cohort = cdm$cohort2,
                                    name = "new_cohort",
                                    cohortId = c("cohort_1"),
                                    matchSex = TRUE,
                                    matchYearOfBirth = TRUE,
                                    ratio = 2))
  expect_true(nrow(settings(c)) == 2)

  # empty set
  cdm <- omopgenerics::emptyCohortTable(cdm, name = "cohort")
  expect_warning(empty_cohort <- matchCohorts(cohort = cdm$cohort, name = "empty_cohort"))
  expect_identical(cdm$cohort |> dplyr::collect(), empty_cohort |> dplyr::collect())

  # expect errors
  expect_error(matchCohorts(cohort = dplyr::tibble()))
  expect_error(matchCohorts(cohort = cdm$cases, ratio = -0.5))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("matchCohorts, no duplicated people within a cohort", {
  skip_on_cran()

  cdm <- omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = "cohort1") |>
    omock::mockCohort(name = "cohort2") |>
    copyCdm()

  # one record per person
  cdm$cohort1 <- cdm$cohort1 |>
    CohortConstructor::requireIsFirstEntry()
  cdm$cohort2 <- cdm$cohort2 |>
    CohortConstructor::requireIsFirstEntry()

  cdm$new_cohort <- matchCohorts(cohort = cdm$cohort1,
                                 name = "new_cohort",
                                 matchSex = TRUE,
                                 matchYearOfBirth = TRUE,
                                 ratio = 1)

  p1 <- cdm$new_cohort |>
    dplyr::filter(cohort_definition_id == 1) |>
    dplyr::select(subject_id) |>
    dplyr::pull()
  expect_true(anyDuplicated(p1) == 0L)


  cdm$cohort2 <- cdm$cohort2 |>
    dplyr::group_by(subject_id) |>
    dplyr::filter(dplyr::n() == 1) |>
    dplyr::ungroup() |>
    dplyr::compute(temporary = FALSE, name = "cohort2")

  cdm$new_cohort <- matchCohorts(cohort = cdm$cohort2,
                                 name = "new_cohort",
                                 matchSex = TRUE,
                                 matchYearOfBirth = TRUE,
                                 ratio = 5)
  p1 <- cdm$new_cohort |>
    dplyr::filter(cohort_definition_id == 2) |>
    dplyr::select(subject_id) |>
    dplyr::pull()

  expect_true(anyDuplicated(p1) == 0L)

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("check that we obtain expected result when ratio is 1", {
  skip_on_cran()

  cdm <- omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = "cohort2", numberCohorts = 2) |>
    copyCdm()

  # Number of counts for the initial cohorts are the same as in the matched cohorts
  matched_cohorts <- matchCohorts(cohort = cdm$cohort2,
                                  name = "new_cohort",
                                  matchSex = TRUE,
                                  matchYearOfBirth = TRUE,
                                  ratio = 1)

  expect_true(nrow(omopgenerics::cohortCount(matched_cohorts) |>
                     dplyr::left_join(omopgenerics::settings(matched_cohorts),
                                      by = "cohort_definition_id") |>
                     dplyr::filter(stringr::str_detect(cohort_name, "cohort_1"))  |>
                     dplyr::select("number_records") |>
                     dplyr::distinct()) == 1)

  expect_true(nrow(omopgenerics::cohortCount(matched_cohorts) |>
                     dplyr::left_join(omopgenerics::settings(matched_cohorts),
                                      by = "cohort_definition_id") |>
                     dplyr::filter(stringr::str_detect(cohort_name, "cohort_2"))  |>
                     dplyr::select("number_records") |>
                     dplyr::distinct()) == 1)

  # Everybody has a match
  n <- matched_cohorts |>
    dplyr::summarise(n = max(.data$cohort_definition_id, na.rm = TRUE)/2) |>
    dplyr::pull()

  cohorts <- matched_cohorts |>
    dplyr::select("person_id" = "subject_id", "cohort_definition_id") |>
    dplyr::inner_join(
      cdm$person |>
        dplyr::select("person_id", "gender_concept_id", "year_of_birth"),
      by = "person_id"
    )

  # THIS WAS NA BECAUSE IT WAS DATA IN THE DB, BUT WHEN LOCAL IS NOT NA
  # IS THERE A COLLECT MISSING SOMEWHERE?
  # expect_true(is.na(nrow(cohorts |>
  #                          dplyr::filter(.data$cohort_definition_id %in% c(1,2,3)) |>
  #                          dplyr::left_join(
  #                            cohorts |>
  #                              dplyr::filter(.data$cohort_definition_id %in% c(4,5,6)) |>
  #                              dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id-n),
  #                            by = c("cohort_definition_id", "gender_concept_id", "year_of_birth")
  #                          ) |>
  #                          dplyr::filter(
  #                            is.na(person_id.y)
  #                          ))))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("test exactMatchingCohort with a ratio bigger than 1", {
  skip_on_cran()

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = dplyr::tibble(
        person_id = 1:10L,
        gender_concept_id = 8532L,
        year_of_birth = 1980L,
        race_concept_id = 0L,
        ethnicity_concept_id = 0L
      ),
      observation_period = dplyr::tibble(
        "observation_period_id" = 1:10L,
        "person_id" = 1:10L,
        "observation_period_start_date" = as.Date(rep("1984-01-01",10)),
        "observation_period_end_date"   = as.Date(rep("2021-01-01",10)),
        "period_type_concept_id"        = 44814724L
      ),
      condition_occurrence = dplyr::tibble(
        condition_occurrence_id = 1:10L,
        person_id = 1:10L,
        "condition_concept_id" = c(317009,317009,4266367,4266367,rep(1,6)),
        "condition_start_date" = as.Date(c("2017-10-30","2003-01-04","2014-12-15",
                                           "2010-09-09","2004-08-26","1985-03-31",
                                           "1985-03-13","1985-07-11","1983-11-07","2020-01-13")),
        "condition_end_date"   = as.Date(c("2017-11-01","2003-01-05","2014-12-16",
                                           "2010-09-10","2004-08-27","1985-04-01",
                                           "1985-03-14","1985-07-12","1983-11-08","2020-01-14")),
        "condition_type_concept_id" = 32020L
      )
    ),
    cdmName = "mock",
    cohortTables = list(
      cohort = dplyr::tibble(
        cohort_definition_id = c(1,1,2,2),
        subject_id = c(1,2,3,4),
        cohort_start_date = as.Date(c("2017-10-30","2003-01-04","2014-12-15","2010-09-09")),
        cohort_end_date = rep(as.Date("2021-01-01"),4)
      )
    )
  ) |>
    copyCdm()

  cdm$new_cohort <- matchCohorts(cohort = cdm$cohort,
                                 name = "new_cohort",
                                 matchSex = TRUE,
                                 matchYearOfBirth = TRUE,
                                 ratio = 4)

  expect_true(
    cdm[["new_cohort"]] |>
      cohortCount() |>
      dplyr::filter(.data$cohort_definition_id %in% omopgenerics::getCohortId(
        cdm$new_cohort, "cohort_1_sampled"
      )) |>
      dplyr::pull("number_subjects") |>
      sum() == 2
  )
  expect_true(
    cdm[["new_cohort"]] |>
      cohortCount() |>
      dplyr::filter(.data$cohort_definition_id %in% omopgenerics::getCohortId(
        cdm$new_cohort, "cohort_1_matched"
      )) |>
      dplyr::pull("number_subjects") |>
      sum() == 8
  )
  expect_true(
    cdm[["new_cohort"]] |>
      cohortCount() |>
      dplyr::filter(.data$cohort_definition_id %in% omopgenerics::getCohortId(
        cdm$new_cohort, "cohort_2_sampled"
      )) |>
      dplyr::pull("number_subjects") |>
      sum() == 2
  )
  expect_true(
    cdm[["new_cohort"]] |>
      cohortCount() |>
      dplyr::filter(.data$cohort_definition_id %in% omopgenerics::getCohortId(
        cdm$new_cohort, "cohort_2_matched"
      )) |>
      dplyr::pull("number_subjects") |>
      sum() == 8
  )

  outc <- cdm[["new_cohort"]] |>
    dplyr::filter(subject_id == 5) |>
    dplyr::pull("cohort_start_date") %in% as.Date(c("2017-10-30","2003-01-04","2014-12-15","2010-09-09"))
  expect_true(unique(outc) == TRUE)

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("keepOriginalCohorts works" , {
  skip_on_cran()

  cdm <- omock::mockPerson(nPerson = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = "cohort2", numberCohorts = 2) |>
    copyCdm()

  cohort <- cdm$cohort2 |> matchCohorts(cohortId = 1, keepOriginalCohorts = TRUE, name = "new_cohort")
  expect_identical(settings(cohort), dplyr::tibble(
      cohort_definition_id = as.integer(1:3),
      cohort_name = c("cohort_1", "cohort_1_sampled", "cohort_1_matched"),
      target_table_name = c(NA, rep("cohort2", 2)),
      target_cohort_id = c(NA, 1L, 1L),
      target_cohort_name = c(NA, "cohort_1_sampled", "cohort_1_sampled"),
      match_sex = c(NA, rep(TRUE, 2)),
      match_year_of_birth = c(NA, rep(TRUE, 2)),
      match_status = c(NA, "target", "control")
    ))
  cohort <- cdm$cohort2 |> matchCohorts(keepOriginalCohorts = TRUE)
  expect_identical(settings(cohort), dplyr::tibble(
      cohort_definition_id = as.integer(1:6),
      cohort_name = c("cohort_1", "cohort_2", "cohort_1_sampled", "cohort_2_sampled", "cohort_1_matched", "cohort_2_matched"),
      target_table_name = c(NA, NA, rep("cohort2", 4)),
      target_cohort_id = c(NA, NA, 1L, 2L, 1L, 2L),
      target_cohort_name = c(NA, NA, "cohort_1_sampled", "cohort_2_sampled", "cohort_1_sampled", "cohort_2_sampled"),
      match_sex = c(NA, NA, rep(TRUE, 4)),
      match_year_of_birth = c(NA, NA, rep(TRUE, 4)),
      match_status = c(NA, NA, "target", "target", "control", "control")
    ))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("test indexes - postgres", {
  skip_on_cran()
  skip_if(!testIndexes)

  if (dbToTest == "postgres CDMConnector") {
    cdm <- omock::mockCdmFromTables(tables = list(
      my_cohort = dplyr::tibble(
        cohort_definition_id = 1L,
        subject_id = 1L,
        cohort_start_date = as.Date("2009-01-02"),
        cohort_end_date = as.Date("2009-01-03"),
        other_date = as.Date("2009-01-01")
      )
    )) |>
      copyCdm()

    con <- CDMConnector::cdmCon(cdm = cdm)

    cdm$my_cohort <- matchCohorts(cdm$my_cohort)

    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

    dropCreatedTables(cdm = cdm)
  }

})
