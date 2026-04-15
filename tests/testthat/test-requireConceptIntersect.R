test_that("test it works", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    subject_id = c(1L, 1L, 2L, 3L, 1L, 1L, 1L, 1L),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15"
    ))
  )

  cohort_2 = dplyr::tibble(
    subject_id = 1:10,
    cohort_definition_id = 1L,
    cohort_start_date = as.Date('2015-01-01'),
    cohort_end_date = as.Date('2015-01-01')
  )

  person <- dplyr::tibble(
    person_id = 1:4L,
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L),
    year_of_birth = c(1997L, 1963L, 1986L, 1978L),
    month_of_birth = c(8L, 1L, 3L, 11L),
    day_of_birth = c(22L, 27L, 10L, 8L),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:10L,
    person_id = 1:10L,
    observation_period_start_date = as.Date(c("2000-06-03", "1999-04-05", "2015-01-15", rep("1989-12-09", 7))),
    observation_period_end_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11", rep("2020-12-31", 7))),
    period_type_concept_id = NA_integer_
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort1" = cohort_1)) |>
    omopgenerics::insertTable(name = "observation_period", table = obs) |>
    omopgenerics::insertTable(name = "person", table = person) |>
    omock::mockVocabularyTables(concept = dplyr::tibble(
      "concept_id" = 1:7L,
      "concept_name" = "my concept",
      "domain_id" = "Drug",
      "vocabulary_id" = NA,
      "concept_class_id" = NA,
      "concept_code" = NA,
      "valid_start_date" = NA,
      "valid_end_date" = NA
    )) |>
    omopgenerics::insertTable(name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = 1:17L,
      "person_id" = as.integer(c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 1,2,2,3,3,3)),
      "drug_concept_id" = as.integer(c(1, 1, 1, 3, 1, 1, 3, 1, 1, 1, 1, rep(2, 6))),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, rep(1803, 6)),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, rep(1803, 6)),
      "drug_type_concept_id" = 1L
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
      )
    ) |>
    copyCdm()
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "cohort2", table = cohort_2)
  cdm$cohort2 <- cdm$cohort2 |> omopgenerics::newCohortTable(.softValidation = TRUE)

  # require flag in concept ----
  start_cols <- colnames(cdm$cohort1)
  cdm$cohort3 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                          conceptSet = list(a = 1L),
                                          window = c(-Inf, Inf),
                                          name = "cohort3")
  expect_identical(colnames(cdm$cohort3), colnames(cdm$cohort1))
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") == 1L))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17", "2004-03-11")))

  cdm$in_obs <- requireConceptIntersect(cohort = cdm$cohort1,
                                        conceptSet = list(a = 1L),
                                        intersections = 2,
                                        window = c(0, Inf),
                                        inObservation = FALSE,
                                        name = "in_obs")
  expect_identical(
    collectCohort(cdm$in_obs, 1),
    dplyr::tibble(subject_id = 2L, cohort_start_date = as.Date("1999-05-03"), cohort_end_date = as.Date("2001-06-15"))
  )

  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events",
                      "Require 1 or more intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Require 1 or more intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date")))
  # cohort Id
  cdm$cohort4 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                          cohortId = 1,
                                          conceptSet = list(a = 1L),
                                          window = c(-Inf, Inf),
                                          name = "cohort4")
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") ==
                    c(rep(1, 6))))
  expect_true(all(cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17", "2004-03-11")))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events",
                      "Require 1 or more intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date",
                      "Initial qualifying events")))
  # censor date
  cdm$cohort5 <- requireConceptIntersect(cohort = cdm$cohort1,
                                         conceptSet = list(a = 1L),
                                         window = c(-Inf, Inf),
                                         censorDate = "cohort_end_date",
                                         name = "cohort5")
  expect_true(cdm$cohort5 |> dplyr::pull("subject_id") |> length() == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events",
                      "Require 1 or more intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date, censoring at cohort_end_date",
                      "Initial qualifying events",
                      "Require 1 or more intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date, censoring at cohort_end_date")))

  # empty concept
  expect_message(
    cdm$cohort1_equal <-  requireConceptIntersect(cohort = cdm$cohort1,
                                                  conceptSet = list(),
                                                  window = list(c(-Inf, Inf)),
                                                  name = "cohort1_equal")
  )
  expect_true(all(omopgenerics::attrition(cdm$cohort1_equal)$reason ==
                    c("Initial qualifying events", "Initial qualifying events")))
  expect_equal(collectCohort(cdm$cohort1_equal,1), collectCohort(cdm$cohort1, 1))

  # requiring absence in another cohort ----
  cdm$cohort3_inclusion <-  requireConceptIntersect(cohort = cdm$cohort1,
                                                    conceptSet = list(a = 1L),
                                                    window = c(-Inf, Inf),
                                                    name = "cohort3_inclusion")
  cdm$cohort3_exclusion <-  requireConceptIntersect(cohort = cdm$cohort1,
                                                    conceptSet = list(a = 1L),
                                                    window = c(-Inf, Inf),
                                                    intersections = 0,
                                                    name = "cohort3_exclusion")
  in_both <- intersect(cdm$cohort3_inclusion |>
                         dplyr::pull("subject_id") |>
                         unique(),
                       cdm$cohort3_exclusion |>
                         dplyr::pull("subject_id") |>
                         unique())
  expect_true(length(in_both) == 0)
  in_both <- intersect(cdm$cohort3_inclusion |>
                         dplyr::pull("cohort_start_date") |>
                         sort(),
                       cdm$cohort3_exclusion |>
                         dplyr::pull("cohort_start_date") |>
                         sort())
  expect_true(length(in_both) == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort3_exclusion)$reason ==
                    c("Initial qualifying events",
                      "Require 0 intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Require 0 intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date")))

  # cohort Id
  cdm$cohort3_exclusion_partial <-  requireConceptIntersect(
    cohort = cdm$cohort1,
    cohortId = "cohort_1",
    conceptSet = list(a = 1L),
    window = c(-Inf, Inf),
    intersections = 0,
    name = "cohort3_exclusion_partial"
  )
  expect_true(all(cdm$cohort3_exclusion_partial |> dplyr::pull("subject_id") |> sort() ==
                    c(1, 1, 1, 1, 2, 3)))
  expect_true(all(cdm$cohort3_exclusion_partial |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-05-03", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2015-02-25")))
  expect_true(all(omopgenerics::attrition(cdm$cohort3_exclusion_partial)$reason ==
                    c("Initial qualifying events",
                      "Require 0 intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date",
                      "Initial qualifying events"
                    )))

  # Different intersection count requirements ----
  # no intersections - people not in cohort2
  expect_identical(
    sort(cdm$cohort2 |>
           requireConceptIntersect(
             intersections = c(0, 0),
             conceptSet = list("a" = 2L),
             window = c(-Inf, Inf),
             name = "cohort2_test",
             inObservation = FALSE
           ) |>
           dplyr::pull("subject_id")),
    as.integer(c(4,5,6,7,8,9,10))
  )

  # only one intersection
  expect_identical(sort(cdm$cohort2 |>
                          requireConceptIntersect(intersections = c(1, 1),
                                                  conceptSet = list("a" = 2L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort2_test", inObservation = FALSE) |>
                          dplyr::pull("subject_id")), c(1L))

  expect_identical(sort(cdm$cohort2 |>
                          requireConceptIntersect(intersections = c(1),
                                                  conceptSet = list("a" = 2L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort2_test", inObservation = FALSE) |>
                          dplyr::pull("subject_id")), c(1L))

  # 2 intersections
  expect_identical(sort(cdm$cohort2 |>
                          requireConceptIntersect(intersections = c(2, 2),
                                                  conceptSet = list("a" = 2L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort2_test", inObservation = FALSE) |>
                          dplyr::pull("subject_id")), c(2L))

  expect_identical(sort(cdm$cohort2 |>
                          requireConceptIntersect(intersections = c(2),
                                                  conceptSet = list("a" = 2L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort2_test", inObservation = FALSE) |>
                          dplyr::pull("subject_id")), c(2L))

  # 2 or more intersections
  expect_identical(sort(cdm$cohort2 |>
                          requireConceptIntersect(intersections = c(2, Inf),
                                                  conceptSet = list("a" = 2L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort2_test", inObservation = FALSE) |>
                          dplyr::pull("subject_id")), c(2L, 3L))

  # 2 or 3 intersections
  expect_identical(
    sort(
      cdm$cohort2 |>
        requireConceptIntersect(
          intersections = c(2L, 3L),
          conceptSet = list("a" = 2L),
          window = c(-Inf, Inf),
          name = "cohort2_test",
          inObservation = FALSE
        ) |>
        dplyr::pull("subject_id")
    ),
    c(2L, 3L)
  )

  # expected errors
  expect_error(expect_warning(
    requireConceptIntersect(cohort = cdm$cohort2,
                            intersections = c(-10, 10),
                            conceptSet = list("a" = 2L),
                            window = c(-Inf, Inf), inObservation = FALSE)
  ))
  expect_error(expect_warning(
    requireConceptIntersect(cohort = cdm$cohort2,
                            intersections = c(11, 10),
                            conceptSet = list("a" = 1L),
                            window = c(-Inf, Inf), inObservation = FALSE)
  ))
  expect_error(expect_warning(
    requireConceptIntersect(cohort = cdm$cohort2,
                            intersections = c(Inf, Inf),
                            conceptSet = list("a" = 1L),
                            window = c(-Inf, Inf))
  ))
  expect_error(expect_warning(
    requireConceptIntersect(cohort = cdm$cohort2,
                            intersections = c(1, 2, 3),
                            conceptSet = list("a" = 1L),
                            window = c(-Inf, Inf))
  ))

  # codelists ----
  cdm$cohort3 <- conceptCohort(cdm, list("a" = 1L, "b" = 2L), name = "cohort3")

  # cohort without codelist
  cdm$cohort4 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                          conceptSet = list("a" = 1L),
                                          intersections = 0,
                                          window = c(-Inf, 0),
                                          name = "cohort4")
  expect_identical(
    attr(cdm$cohort4, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = 1:2L,
      codelist_name = "a",
      concept_id = 1L,
      codelist_type = "inclusion criteria"
    )
  )

  # cohort with codelist
  cdm$cohort4 <-  requireConceptIntersect(cohort = cdm$cohort3,
                                          conceptSet = list("a" = 1L),
                                          window = c(-Inf, Inf),
                                          name = "cohort4")
  expect_identical(
    attr(cdm$cohort4, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1L, 1L, 2L, 2L),
      codelist_name = c("a", "a", "b", "a"),
      concept_id = c(1L, 1L, 2L, 1L),
      codelist_type = c("index event", "inclusion criteria", "index event", "inclusion criteria")
    )
  )

  # combinations ----
  # subject 5: 1 record in drugs 4 5 6, before cohort 4 entries
  # subject 6: 3 records in 4, 2 in 5, 0 in 6, before cohort 4 entries
  # subject 7: 2 records in 4, 2 in 5, before and after cohort 4
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = 1:12L,
    drug_concept_id = c(4, 5, 6, 4, 4, 4, 5, 5, 4, 4, 5, 5) |> as.integer(),
    person_id = c(5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7) |> as.integer(),
    drug_exposure_start_date = c(
      "2018-01-01", "2018-01-01", "2018-01-01", "2018-01-01", "2018-05-01",
      "2018-06-01", "2018-01-01", "2018-05-01", "2019-01-04", "2019-01-05",
      "2019-01-06", "2019-01-07"
    ) |> as.Date(),
    drug_exposure_end_date = c(
      "2018-01-01", "2018-01-01", "2018-01-01", "2018-01-01", "2018-05-01",
      "2018-06-01", "2018-01-01", "2018-05-01", "2019-01-04", "2019-01-05",
      "2019-01-06", "2019-01-07"
    ) |> as.Date(),
    drug_type_concept_id = 1L
  )
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "drug_exposure", table = drug_exposure)
  cohort4 <- dplyr::tibble(
    subject_id = c(5,6,6,7,7,7) |> as.integer(),
    cohort_definition_id = 1L,
    cohort_start_date = c(
      as.Date('2019-01-01'), as.Date('2019-01-02'), as.Date('2019-01-03'),
      as.Date('2019-01-04'), as.Date('2019-01-05'), as.Date('2019-01-06')
    ),
    cohort_end_date =  c(
      as.Date('2019-01-01'), as.Date('2019-01-02'), as.Date('2019-01-03'),
      as.Date('2019-01-04'), as.Date('2019-01-05'), as.Date('2019-01-06')
    )
  )
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "cohort4", table = cohort4)
  cdm$cohort4 <- cdm$cohort4 |> omopgenerics::newCohortTable()

  # any - >=1 intersecction
  cdm$cohort5 <- cdm$cohort4 |>
    requireConceptIntersect(
      conceptSet = list("a" = 4L, "b" = 5L, "c" = 6L),
      window = c(-Inf, -5),
      intersections = c(1, Inf),
      cohortCombinationCriteria = c(1, Inf),
      name = "cohort5"
    )
  expect_equal(
    collectCohort(cdm$cohort5, 1),
    dplyr::tibble(
      subject_id = c(5, 6, 6) |> as.integer(),
      cohort_start_date = c("2019-01-01", "2019-01-02", "2019-01-03") |> as.Date(),
      cohort_end_date = c("2019-01-01", "2019-01-02", "2019-01-03") |> as.Date()
    )
  )
  expect_equal(
    attrition(cdm$cohort5)$reason[2],
    "Require 1 or more intersections for 1 or more of the concept sets: a, b and c. Intersection window: -Inf to -5 days relative to cohort_start_date"
  )

  cdm$cohort5b <- cdm$cohort4 |>
    requireConceptIntersect(
      conceptSet = list("a" = 4L, "b" = 5L, "c" = 6L),
      window = c(-Inf, -5),
      intersections = c(1, Inf),
      cohortCombinationCriteria = "any",
      name = "cohort5b"
    )
  expect_identical(cdm$cohort5 |> dplyr::collect(),
                   cdm$cohort5b |> dplyr::collect())

  # all, >=2 intersecttion
  cdm$cohort5 <- cdm$cohort4 |>
    dplyr::mutate(new_date = cohort_end_date) |>
    requireConceptIntersect(
      conceptSet = list("a" = 4L, "b" = 5L, "c" = 6L),
      window = c(-Inf, -5),
      intersections = c(1, Inf),
      cohortCombinationCriteria = 3,
      censorDate = "new_date",
      name = "cohort5"
    )
  expect_equal(
    collectCohort(cdm$cohort5, 1),
    dplyr::tibble(
      subject_id = c(5) |> as.integer(),
      cohort_start_date = c("2019-01-01") |> as.Date(),
      cohort_end_date = c("2019-01-01") |> as.Date()
    )
  )
  expect_equal(
    attrition(cdm$cohort5)$reason[2],
    "Require 1 or more intersections for all 3 concept sets: a, b and c. Intersection window: -Inf to -5 days relative to cohort_start_date, censoring at new_date"
  )

  cdm$cohort5b <- cdm$cohort4 |>
    dplyr::mutate(new_date = cohort_end_date) |>
    requireConceptIntersect(
      conceptSet = list("a" = 4L, "b" = 5L, "c" = 6L),
      window = c(-Inf, -5),
      intersections = c(1, Inf),
      cohortCombinationCriteria = "all",
      censorDate = "new_date",
      name = "cohort5b"
    )
  expect_identical(cdm$cohort5 |> dplyr::collect(),
                   cdm$cohort5b |> dplyr::collect())

  # at first
  cdm$cohort5 <- cdm$cohort4 |>
    requireConceptIntersect(
      conceptSet = list("a" = 4L, "b" = 5L, "c" = 6L),
      window = c(0, Inf),
      intersections = 0,
      cohortCombinationCriteria = 3,
      atFirst = TRUE,
      name = "cohort5"
    )
  expect_equal(
    collectCohort(cdm$cohort5, 1),
    dplyr::tibble(
      subject_id = c(5, 6, 6) |> as.integer(),
      cohort_start_date = c("2019-01-01", "2019-01-02", "2019-01-03") |> as.Date(),
      cohort_end_date = c("2019-01-01", "2019-01-02", "2019-01-03") |> as.Date()
    )
  )
  expect_equal(
    attrition(cdm$cohort5)$reason[2],
    "Require 0 intersections for all 3 concept sets: a, b and c. Intersection window: 0 to Inf days relative to cohort_start_date. Requirement applied to the first entry"
  )


  # name ----
  expect_warning(
    cdm$cohort1 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                            conceptSet = list(a = 1L),
                                            window = c(-Inf, Inf))
  )
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "Require 1 or more intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Require 1 or more intersections with concept set a. Intersection window: -Inf to Inf days relative to cohort_start_date")))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("test indexes - postgres, atFirst", {
  skip_on_cran()
  skip_if(!testIndexes)

  if (dbToTest == "postgres CDMConnector") {
    cdm <- omock::mockCdmFromTables(tables = list(
      my_cohort = data.frame(
        cohort_definition_id = 1L,
        subject_id = 1L,
        cohort_start_date = as.Date("2009-01-02"),
        cohort_end_date = as.Date("2009-01-03"),
        other_date = as.Date("2009-01-01")
      )
    )) |>
      copyCdm()

    con <- CDMConnector::cdmCon(cdm = cdm)

    omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::contains("og_"))

    expect_no_error(cdm$my_cohort |> head(1))
    cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
    expect_no_error(omopgenerics::settings(cdm$my_cohort))
    expect_warning(
      cdm$my_cohort <- requireConceptIntersect(cdm$my_cohort,
                                               conceptSet = list(a = 0),
                                               window = list(c(0, Inf)))
    )
    expect_no_error(cdm$my_cohort |> head(1))
    expect_no_error(omopgenerics::settings(cdm$my_cohort))
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_test_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_test_my_cohort_subject_id_cohort_start_date_idx ON public.cc_test_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    # atFirst
    cohort <- dplyr::tibble(
      cohort_definition_id = c(rep(1L, 4), rep(2L, 4)),
      subject_id = c(1L, 1L, 2L, 3L, rep(1L, 4)),
      cohort_start_date = as.Date(c(
        "2008-05-17", "2009-03-11", "2010-05-03", "2010-02-25",
        "2008-03-24", "2008-11-28", "2010-01-30", "2009-06-13"
      )),
      cohort_end_date = as.Date(c(
        "2009-03-10", "2009-07-19", "2010-06-15", "2010-04-30",
        "2008-11-27", "2008-01-29", "2010-06-12", "2010-01-15"
      ))
    )
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = "my_cohort",
                                     table = cohort)
    cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort, .softValidation = TRUE)
    cdm$my_cohort_1 <- requireConceptIntersect(cohort = cdm$my_cohort,
                                               conceptSet = list(a = 22340),
                                               window = list(c(0, 365)),
                                               atFirst = TRUE,
                                               name = "my_cohort_1")
    expect_equal(
      collectCohort(cdm$my_cohort_1, 2),
      dplyr::tibble(
        subject_id = 1L,
        cohort_start_date = as.Date(NULL),
        cohort_end_date = as.Date(NULL)
      )
    )
    expect_equal(
      attrition(cdm$my_cohort_1)$reason,
      c('Initial qualifying events',
        'Concept a between 0 & 365 days relative to cohort_start_date between 1 and Inf. Requirement applied to the first entry',
        'Initial qualifying events',
        'Concept a between 0 & 365 days relative to cohort_start_date between 1 and Inf. Requirement applied to the first entry'
      ))

    expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

    dropCreatedTables(cdm = cdm)
  }

})
