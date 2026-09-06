exampleCohortDefinitionCode <- paste0(
  'cdm$my_cohort <- conceptCohort(cdm = cdm, name = "my_cohort", ',
  'conceptSet = codelist["t1dm"]) |> ',
  'requireSex(sex = "Male") |> ',
  'requireConceptIntersect(conceptSet = codelist["t2dm"], ',
  'window = c(-Inf, 0))'
)

test_that("cohort definitions can be created from and converted to code", {
  definition <- cohortDefinitionFromCode(exampleCohortDefinitionCode)
  expected_steps <- list(
    list(
      package = "CohortConstructor",
      fun = "conceptCohort",
      parameters = list(conceptSet = "t1dm")
    ),
    list(
      package = "CohortConstructor",
      fun = "requireSex",
      parameters = list(sex = "Male")
    ),
    list(
      package = "CohortConstructor",
      fun = "requireConceptIntersect",
      parameters = list(conceptSet = "t2dm", window = c(-Inf, 0))
    )
  )

  expect_s3_class(definition, "cohort_definition")
  expect_named(definition, "my_cohort")
  expect_identical(definition$my_cohort$name, "my_cohort")
  expect_identical(definition$my_cohort$definition, expected_steps)
  expect_identical(
    definition$my_cohort$definition[[1]]$parameters$conceptSet,
    "t1dm"
  )
  expect_identical(
    definition$my_cohort$definition[[2]]$parameters$sex,
    "Male"
  )
  expect_identical(
    definition$my_cohort$definition[[3]]$parameters$conceptSet,
    "t2dm"
  )
  expect_equal(
    definition$my_cohort$definition[[3]]$parameters$window,
    c(-Inf, 0)
  )
  expect_identical(definition$my_cohort$needed_codelists, c("t1dm", "t2dm"))
  expect_identical(definition$my_cohort$needed_cohorts, character())

  generated_code <- codeFromCohortDefinition(definition)
  expect_identical(generated_code, exampleCohortDefinitionCode)
  expect_equal(
    cohortDefinitionFromCode(generated_code),
    definition,
    ignore_attr = FALSE
  )
})

test_that("cohort definitions round-trip through JSON", {
  definition <- cohortDefinitionFromCode(exampleCohortDefinitionCode)
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  exportCohortDefinition(definition, path)
  expect_true(file.exists(file.path(path, "my_cohort.json")))

  imported <- importCohortDefinition(path)
  expect_s3_class(imported, "cohort_definition")
  expect_identical(imported$my_cohort$name, "my_cohort")
  expect_equal(
    imported$my_cohort$definition[[3]]$parameters$window,
    c(-Inf, 0)
  )
  expect_identical(imported$my_cohort$needed_codelists, c("t1dm", "t2dm"))
})

test_that("invalid JSON files do not abort an import", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  writeLines("{", file.path(path, "invalid.json"))

  expect_message(
    imported <- importCohortDefinition(path),
    regexp = "parse error|unexpected end|JSON"
  )
  expect_s3_class(imported, "cohort_definition")
  expect_length(imported, 0)
})

test_that("cohort definitions can be instantiated on a CDM", {
  skip_on_cran()

  cdm <- omock::mockVocabularySet() |>
    omock::mockCdmFromTables(tables = list(
      condition_occurrence = dplyr::tibble(
        condition_occurrence_id = 1:5L,
        person_id = 1:5L,
        condition_concept_id = 35208414L,
        condition_start_date = as.Date("2020-01-01"),
        condition_end_date = as.Date("2020-01-01"),
        condition_type_concept_id = 32817L
      )
    ))
  definition <- cohortDefinitionFromCode(exampleCohortDefinitionCode)
  codelist <- list(t1dm = 35208414L, t2dm = 35208414L)

  expect_warning(
    cohort <- instantiateCohortDefinition(
      cohortDefinition = definition,
      cdm = cdm,
      name = "instantiated_cohort",
      conceptSet = codelist
    ),
    regexp = "name.*provided"
  )
  expect_s3_class(cohort, "cohort_table")
  expect_identical(omopgenerics::tableName(cohort), "instantiated_cohort")
  expect_true(omopgenerics::cohortCount(cohort)$number_subjects >= 0)

  expect_error(
    instantiateCohortDefinition(
      cohortDefinition = definition,
      cdm = cdm,
      name = "missing_codelist",
      conceptSet = list(t1dm = 35208414L)
    ),
    regexp = "t2dm"
  )
})

test_that("all cohort definitions are instantiated and bound together", {
  skip_on_cran()

  cdm <- omock::mockVocabularySet() |>
    omock::mockCdmFromTables(tables = list(
      condition_occurrence = dplyr::tibble(
        condition_occurrence_id = 1:5L,
        person_id = 1:5L,
        condition_concept_id = 35208414L,
        condition_start_date = as.Date("2020-01-01"),
        condition_end_date = as.Date("2020-01-01"),
        condition_type_concept_id = 32817L
      )
    ))

  cohort1 <- cohortDefinitionFromCode(paste0(
    'cdm$cohort1 <- conceptCohort(cdm = cdm, name = "cohort1", ',
    'conceptSet = codelist["t1dm"]) |> ',
    'requireCohortIntersect(targetCohortTable = "cohort2", ',
    'window = c(-Inf, 0))'
  ))
  cohort2 <- cohortDefinitionFromCode(paste0(
    'cdm$cohort2 <- conceptCohort(cdm = cdm, name = "cohort2", ',
    'conceptSet = codelist["t2dm"])'
  ))
  definitions <- c(cohort1, cohort2)
  expect_identical(definitions$cohort1$needed_cohorts, "cohort2")

  codelist <- list(t1dm = 35208414L, t2dm = 35208414L)
  expect_warning(
    cohort <- instantiateCohortDefinition(
      cohortDefinition = definitions,
      cdm = cdm,
      name = "combined_cohort",
      conceptSet = codelist
    ),
    regexp = "name.*provided"
  )
  expect_s3_class(cohort, "cohort_table")
  expect_identical(omopgenerics::tableName(cohort), "combined_cohort")
  expect_setequal(
    omopgenerics::settings(cohort)$cohort_name,
    c("t1dm", "t2dm")
  )

  expect_error(
    instantiateCohortDefinition(
      cohortDefinition = cohort1,
      cdm = cdm,
      name = "missing_dependency",
      conceptSet = codelist
    ),
    regexp = "cohort2"
  )
})
