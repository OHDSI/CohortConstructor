test_that("requiring presence in another cohort", {
  skip_on_cran()

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(rep(1L, 4), rep(2L, 4)),
    subject_id = c(1L, 1L, 2L, 3L, rep(1L, 4)),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15"
    ))
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(rep(1L, 4), rep(2L, 4)),
    subject_id = c(1L, 2L, 2L, 3L, 1L, 1L, 2L, 3L),
    cohort_start_date = as.Date(c(
      "2003-05-08", "2000-01-11", "2000-05-28", "2015-01-25",
      "2000-06-17", "2004-12-12", "1999-07-11", "2015-02-02"
    )),
    cohort_end_date = as.Date(c(
      "2005-04-08", "2000-05-27", "2001-09-08", "2015-04-26",
      "2004-12-11", "2007-09-06", "2002-03-26", "2015-08-12"
    ))
  )
  cohort3 <- dplyr::tibble(
    subject_id = 1:10L,
    cohort_definition_id = 1L,
    cohort_start_date = as.Date('2020-01-01'),
    cohort_end_date = as.Date('2020-01-01'))
  cohort4 <- dplyr::tibble(
    subject_id = c(1,2,2,3,3,3) |> as.integer(),
    cohort_definition_id = 1L,
    cohort_start_date = c(as.Date('2019-01-01'),
                          as.Date('2019-01-02'),
                          as.Date('2019-01-03'),
                          as.Date('2019-01-04'),
                          as.Date('2019-01-05'),
                          as.Date('2019-01-06')),
    cohort_end_date =  c(as.Date('2019-01-01'),
                         as.Date('2019-01-02'),
                         as.Date('2019-01-03'),
                         as.Date('2019-01-04'),
                         as.Date('2019-01-05'),
                         as.Date('2019-01-06'))
  )

  cdm <- omock::mockCdmFromTables(
    tables = list("cohort1" = cohort1, "cohort2" = cohort2, "cohort3" = cohort3, "cohort4" = cohort4)
  ) |>
    copyCdm()

  # simple examples ----
  start_cols <- colnames(cdm$cohort1)
  cdm$cohort3 <-  requireCohortIntersect(cohort = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 1,
                                         window = c(-Inf, Inf),
                                         name = "cohort3")
  expect_identical(colnames(cdm$cohort3), colnames(cdm$cohort1))

  expect_true(all(
    cdm$cohort3  |>
      dplyr::distinct(subject_id) |>
      dplyr::pull() %in%
      intersect(
        cdm$cohort1 |>
          dplyr::distinct(subject_id) |>
          dplyr::pull(),
        cdm$cohort2 |>
          dplyr::filter(cohort_definition_id == 1) |>
          dplyr::distinct(subject_id) |>
          dplyr::pull()
      )
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_1 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times",
                      "Initial qualifying events",
                      "In cohort cohort_1 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times")))

  cdm$cohort4 <-  requireCohortIntersect(cohort = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 2,
                                         window = list(c(-Inf, Inf)),
                                         name = "cohort4")
  expect_true(all(cdm$cohort4 |>
                    dplyr::distinct(subject_id) |>
                    dplyr::pull() %in%
                    intersect(cdm$cohort1 |>
                                dplyr::distinct(subject_id) |>
                                dplyr::pull(),
                              cdm$cohort2 |>
                                dplyr::filter(cohort_definition_id == 2) |>
                                dplyr::distinct(subject_id) |>
                                dplyr::pull())))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times",
                      "Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times")))

  # name
  cdm$cohort1 <-  requireCohortIntersect(cohort = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 2,
                                         window = c(-Inf, Inf))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times",
                      "Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times")))

  # censor date
  cdm$cohort5 <- requireCohortIntersect(cohort = cdm$cohort2,
                                        targetCohortTable = "cohort1",
                                        targetCohortId = 2,
                                        window = c(0, Inf),
                                        censorDate = "cohort_end_date",
                                        name = "cohort5")
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_start_date") == c("2003-05-08", "2000-06-17", "2004-12-12")))
  expect_true(all(cdm$cohort5 |> dplyr::pull("subject_id") == c("1", "1", "1")))
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_definition_id") == c("1", "2", "2")))
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between 0 & Inf days relative to cohort_start_date between 1 and Inf times, censoring at cohort_end_date",
                      "Initial qualifying events",
                      "In cohort cohort_2 between 0 & Inf days relative to cohort_start_date between 1 and Inf times, censoring at cohort_end_date")))

  # cohort Id
  cdm$cohort6 <- requireCohortIntersect(cohort = cdm$cohort2,
                                        cohortId = "cohort_2",
                                        targetCohortTable = "cohort1",
                                        targetCohortId = 1,
                                        window = c(0, Inf),
                                        censorDate = "cohort_end_date",
                                        name = "cohort6")
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-07-11", "2000-01-11", "2000-05-28", "2000-06-17",
                      "2003-05-08", "2004-12-12", "2015-01-25", "2015-02-02")))
  expect_true(all(cdm$cohort6 |> dplyr::pull("subject_id") |> sort() == c("1","1","1", "2", "2", "2", "3", "3")))
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_definition_id") |> sort() == c(rep("1", 4), rep("2", 4))))
  expect_true(all(omopgenerics::attrition(cdm$cohort6)$reason ==
                    c("Initial qualifying events",
                      "Initial qualifying events",
                      "In cohort cohort_1 between 0 & Inf days relative to cohort_start_date between 1 and Inf times, censoring at cohort_end_date")))

  cdm$cohort7 <- requireCohortIntersect(cohort = cdm$cohort2,
                                        intersections = c(0,0),
                                        cohortId = 2,
                                        targetCohortTable = "cohort1",
                                        targetCohortId = 1,
                                        window = c(0, Inf),
                                        censorDate = "cohort_end_date",
                                        name = "cohort7")
  expect_true(all(cdm$cohort7 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2000-01-11", "2000-05-28", "2003-05-08", "2015-01-25")))
  expect_true(all(cdm$cohort7 |> dplyr::pull("subject_id") |> sort() == c("1", "2", "2", "3")))
  expect_true(all(cdm$cohort7 |> dplyr::pull("cohort_definition_id") |> sort() == c(rep("1", 4))))
  expect_true(all(omopgenerics::attrition(cdm$cohort7)$reason ==
                    c("Initial qualifying events",
                      "Initial qualifying events",
                      "Not in cohort cohort_1 between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date")))

  # Require absence ----
  cdm$cohort3_inclusion <-  requireCohortIntersect(cohort = cdm$cohort1,
                                                   targetCohortTable = "cohort2",
                                                   targetCohortId = 1,
                                                   window = c(-Inf, Inf),
                                                   name = "cohort3_inclusion")
  cdm$cohort3_exclusion <-  requireCohortIntersect(cohort = cdm$cohort1,
                                                   intersections = c(0, 0),
                                                   targetCohortTable = "cohort2",
                                                   targetCohortId = 1,
                                                   window = c(-Inf, Inf),
                                                   name = "cohort3_exclusion")
  in_both <- intersect(cdm$cohort3_inclusion |>
                         dplyr::pull("subject_id") |>
                         unique(),
                       cdm$cohort3_exclusion |>
                         dplyr::pull("subject_id") |>
                         unique())
  expect_true(length(in_both) == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort3_exclusion)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times",
                      "Not in cohort cohort_1 between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times",
                      "Not in cohort cohort_1 between -Inf & Inf days relative to cohort_start_date")))

  # empty target cohort
  expect_message(
    cdm$cohort1_equal <-  requireCohortIntersect(cohort = cdm$cohort1,
                                                 targetCohortTable = "cohort3_exclusion",
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 name = "cohort1_equal")
  )

  # Intersecction range ----
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "cohort3", table = cohort3)
  cdm$cohort3 <- cdm$cohort3 |> omopgenerics::newCohortTable()
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "cohort4", table = cohort4)
  cdm$cohort4 <- cdm$cohort4 |> omopgenerics::newCohortTable()
  # no intersections - people not in cohort2
  expect_identical(sort(cdm$cohort3 |>
                          requireCohortIntersect(intersections = c(0, 0),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort4",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), as.integer(c(4,5,6,7,8,9,10)))


  # only one intersection
  expect_identical(sort(cdm$cohort3 |>
                          requireCohortIntersect(intersections = c(1, 1),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort4",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(1L))

  expect_identical(sort(cdm$cohort3 |>
                          requireCohortIntersect(intersections = c(1),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort4",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(1L))

  # 2 intersections
  expect_identical(sort(cdm$cohort3 |>
                          requireCohortIntersect(intersections = c(2, 2),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort4",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L))

  expect_identical(sort(cdm$cohort3 |>
                          requireCohortIntersect(intersections = c(2),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort4",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L))


  # 2 or more intersections
  expect_identical(sort(cdm$cohort3 |>
                          requireCohortIntersect(intersections = c(2, Inf),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort4",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L, 3L))

  # 2 or 3 intersections
  expect_identical(sort(cdm$cohort3 |>
                          requireCohortIntersect(intersections = c(2, 3),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort4",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L, 3L))


  # codelists ----
  cdm$cohort2 <- conceptCohort(cdm, list("a" = 194152L, "b" = 4151660L), name = "cohort2")
  # Only inclusion codes
  cdm$cohort5 <-  requireCohortIntersect(cohort = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 1,
                                         window = c(-Inf, Inf),
                                         name = "cohort5")
  expect_identical(
    attr(cdm$cohort5, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = 1:2,
      codelist_name = "a",
      concept_id = 194152L,
      codelist_type = "inclusion criteria"
    )
  )

  # no inlcusion codes
  cdm$cohort6 <-  requireCohortIntersect(cohort = cdm$cohort2,
                                         targetCohortTable = "cohort1",
                                         targetCohortId = 1,
                                         window = c(-Inf, Inf),
                                         name = "cohort6")
  expect_identical(
    attr(cdm$cohort6, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = 1:2L,
      codelist_name = c("a", "b"),
      concept_id = c(194152L, 4151660L),
      codelist_type = "index event"
    )
  )

  # target cohort empty ----
  # First, make the target cohort empty by filtering out the target cohort ID
  cdm$cohort2 <- cdm$cohort2 |> dplyr::filter(cohort_definition_id != 1)

  # intersections = c(1, Inf) - should exclude all subjects when target is empty
  cdm$cohort_res_1 <-  requireCohortIntersect(
    cohort = cdm$cohort3,
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    window = c(-Inf, Inf),
    intersections = c(1, Inf),
    name = "cohort_res_1"
  )
  expect_true(cdm$cohort_res_1 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(
    "In cohort a between -Inf & Inf days relative to cohort_start_date between 1 and Inf times" %in%
      (omopgenerics::attrition(cdm$cohort_res_1) |> dplyr::pull(reason))
  )

  # intersections = c(0, 0) - should keep all subjects when target is empty
  cdm$cohort_res_2 <- requireCohortIntersect(
    cohort = cdm$cohort1,
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    window = c(-Inf, Inf),
    intersections = c(0, 0),
    name = "cohort_res_2"
  )
  # Check that all subjects are kept when intersections = c(0, 0) and target is empty
  expect_equal(
    cdm$cohort1 |> dplyr::tally() |> dplyr::pull("n"),
    cdm$cohort_res_2 |> dplyr::tally() |> dplyr::pull("n")
  )
  expect_true(
    "Not in cohort a between -Inf & Inf days relative to cohort_start_date" %in%
      (omopgenerics::attrition(cdm$cohort_res_2) |> dplyr::pull(reason))
  )

  # Test that attrition is always recorded when target cohort is empty
  expect_message(
    cdm$cohort_res_3 <- requireCohortIntersect(
      cohort = cdm$cohort4,
      targetCohortTable = "cohort2",
      targetCohortId = 1,
      window = c(-Inf, Inf),
      name = "cohort_res_3"
    )
  )
  # With default intersections = c(1, Inf), all subjects should be excluded when target is empty
  expect_true(cdm$cohort_res_3 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(
    "In cohort a between -Inf & Inf days relative to cohort_start_date between 1 and Inf times" %in%
      (omopgenerics::attrition(cdm$cohort_res_3) |> dplyr::pull(reason))
  )

  # >1 cohort ----
  # subject 1: 1 record in cohorts 1 2 3, before cohort 4 entries
  # subject 2: 3 records in 1, 2 in 2, 0 in 3, before cohort 4 entries
  # subject 3: 2 records in 1, 2 in 2, before and after cohort 4
  cohort_intersect <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3, 1, 1, 1, 2, 2, 1, 1, 2, 2) |> as.integer(),
    subject_id = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3) |> as.integer(),
    cohort_start_date = c(
      "2018-01-01", "2018-01-01", "2018-01-01", "2018-01-01", "2018-05-01",
      "2018-06-01", "2018-01-01", "2018-05-01", "2019-01-04", "2019-01-05",
      "2019-01-06", "2019-01-07"
    ) |> as.Date(),
    cohort_end_date = c(
      "2018-01-01", "2018-01-01", "2018-01-01", "2018-01-01", "2018-05-01",
      "2018-06-01", "2018-01-01", "2018-05-01", "2019-01-04", "2019-01-05",
      "2019-01-06", "2019-01-07"
    ) |> as.Date()
  )
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "cohort_intersect", table = cohort_intersect)
  cdm$cohort_intersect <- cdm$cohort_intersect |> omopgenerics::newCohortTable()

  # any - >=1 intersecction
  cdm$cohort5 <- cdm$cohort4 |>
    requireCohortIntersect(
      targetCohortTable = "cohort_intersect",
      window = c(-Inf, -1),
      intersections = c(1, Inf),
      cohortCombinationRange = c(1, Inf),
      name = "cohort5"
    )
  expect_equal(
    collectCohort(cdm$cohort5, 1),
    dplyr::tibble(
      subject_id = c(1, 2, 2) |> as.integer(),
      cohort_start_date = c("2019-01-01", "2019-01-02", "2019-01-03") |> as.Date(),
      cohort_end_date = c("2019-01-01", "2019-01-02", "2019-01-03") |> as.Date()
    )
  )
  expect_equal(
    attrition(cdm$cohort5)$reason[2],
    "Require 1 or more intersections for 1 or more of the cohorts: cohort_1, cohort_2 and cohort_3. Intersection window: -Inf to -1 days relative to cohort_start_date"
  )

  # all, >=2 intersecttion
  cdm$cohort5 <- cdm$cohort4 |>
    dplyr::mutate(new_date = cohort_end_date) |>
    requireCohortIntersect(
      targetCohortTable = "cohort_intersect",
      window = c(-Inf, -1),
      intersections = c(1, Inf),
      targetCohortId = NULL,
      cohortCombinationRange = 3,
      censorDate = "new_date",
      name = "cohort5"
    )
  expect_equal(
    collectCohort(cdm$cohort5, 1),
    dplyr::tibble(
      subject_id = c(1) |> as.integer(),
      cohort_start_date = c("2019-01-01") |> as.Date(),
      cohort_end_date = c("2019-01-01") |> as.Date()
    )
  )
  expect_equal(
    attrition(cdm$cohort5)$reason[2],
    "Require 1 or more intersections for all 3 cohorts: cohort_1, cohort_2 and cohort_3. Intersection window: -Inf to -1 days relative to cohort_start_date, censoring at new_date"
  )

  # at first
  cdm$cohort5 <- cdm$cohort4 |>
    requireCohortIntersect(
      targetCohortTable = "cohort_intersect",
      window = c(0, Inf),
      intersections = 0,
      targetCohortId = NULL,
      cohortCombinationRange = 3,
      atFirst = TRUE,
      name = "cohort5"
    )
  expect_equal(
    collectCohort(cdm$cohort5, 1),
    dplyr::tibble(
      subject_id = c(1, 2, 2) |> as.integer(),
      cohort_start_date = c("2019-01-01", "2019-01-02", "2019-01-03") |> as.Date(),
      cohort_end_date = c("2019-01-01", "2019-01-02", "2019-01-03") |> as.Date()
    )
  )
  expect_equal(
    attrition(cdm$cohort5)$reason[2],
    "Require 0 intersections for all 3 cohorts: cohort_1, cohort_2 and cohort_3. Intersection window: 0 to Inf days relative to cohort_start_date. Requirement applied to the first entry"
  )

  # expected errors ----
  # only support one target id
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort2",
                                      targetCohortId = c(1,2),
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort2",
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort22", # does not exist
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort2",
                                      targetCohortId = 10, # does not exist
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort2",
                                      targetCohortId = NULL, # only one id supported
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = c("not_a_cohort", "lala"),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      intersections = c(-10, 10),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf),
                                      targetCohortTable = "cohort2"))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      intersections = c(11, 10),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf),
                                      targetCohortTable = "cohort2"))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      intersections = c(Inf, Inf),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf),
                                      targetCohortTable = "cohort2"))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      intersections = c(1, 2, 3),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf),
                                      targetCohortTable = "cohort2"))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  dropCreatedTables(cdm = cdm)
})

test_that("test indexes - postgres, and atFirst", {
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

    omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::contains("og_"))

    cdm$my_cohort <- requireCohortIntersect(cdm$my_cohort, targetCohortTable = "my_cohort", window = list(c(-Inf, Inf)))

    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
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
    cdm$my_cohort_1 <- requireCohortIntersect(cohort = cdm$my_cohort,
                                              intersections = c(1,Inf),
                                              cohortId = 2,
                                              targetCohortTable = "my_cohort",
                                              targetCohortId = 1,
                                              window = c(-365, -1),
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
        'Initial qualifying events',
        'In cohort cohort_1 between -365 & -1 days relative to cohort_start_date between 1 and Inf times. Requirement applied to the first entry'
      ))

    expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

    dropCreatedTables(cdm = cdm)
  }
})
