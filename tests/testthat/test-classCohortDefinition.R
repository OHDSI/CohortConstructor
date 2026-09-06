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
