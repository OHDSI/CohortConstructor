test_that("benchmark works", {
  skip_on_cran()
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == ""){
    Sys.setenv("EUNOMIA_DATA_FOLDER" = file.path(tempdir(), "eunomia"))}
  if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))){ dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
    CDMConnector::downloadEunomiaData()
  }
  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir("synpuf-1k", "5.3"))
  cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema   = "main", writeSchema = "main")
  res <- benchmarkCohortConstructor(cdm)
  expect_false(
    any(omopgenerics::listSourceTables(cdm) == "benchmark")
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "summarise_cohort_overlap") |>
      dplyr::filter(.data$variable_name == "In both cohorts") |>
      omopgenerics::tidy() |>
      dplyr::pull("count") |>
      sort(),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 56, 86, 112, 126, 145, 321)
  )
  expect_equal(
    settings(res) |> dplyr::pull("result_type") |> unique() |> sort(),
    c('cohort_attrition', 'cohort_count', 'instanciation_time', 'omop_counts', 'summarise_cohort_overlap', 'summarise_cohort_timing')
  )
  expect_equal(
    res |> omopgenerics::filterSettings(result_type == "instanciation_time") |> visOmopResults::tidy() |> colnames(),
    c("cdm_name", "tool", "variable_name", "variable_level", "time")
  )
  expect_equal(
    res |> omopgenerics::filterSettings(result_type == "instanciation_time") |> dplyr::pull("variable_level") |> unique(),
    c(NA, "COVID-19 strata", "Overall")
  )
  resAtlas <- benchmarkCohortConstructor(cdm, runCohortConstructorDefinition = FALSE, runCohortConstructorDomain = FALSE)
  expect_equal(
    settings(resAtlas) |> dplyr::pull("result_type") |> unique() |> sort(),
    c('cohort_attrition', 'cohort_count', 'instanciation_time', 'omop_counts')
  )
  expect_equal(
    resAtlas |> omopgenerics::filterSettings(result_type == "instanciation_time") |> dplyr::pull("group_level") |> unique(),
    "CIRCE"
  )
  expect_equal(
    resAtlas |> omopgenerics::filterSettings(result_type == "omop_counts") |> dplyr::pull("group_level"),
    c('person', 'drug_exposure', 'condition_occurrence', 'procedure_occurrence', 'visit_occurrence', 'observation_period', 'measurement', 'observation', 'death')
  )
  resCC <- benchmarkCohortConstructor(cdm, runCIRCE = FALSE)
  expect_equal(
    settings(resCC) |> dplyr::pull("result_type") |> unique() |> sort(),
    c('cohort_attrition', 'cohort_count', 'instanciation_time', 'omop_counts')
  )
  expect_equal(
    resCC |> omopgenerics::filterSettings(result_type == "cohort_count") |> dplyr::pull("group_level") |> unique(),
    c('cc_asthma_no_copd', 'cc_beta_blockers_hypertension', 'cc_covid', 'cc_endometriosis_procedure',
      'cc_hospitalisation', 'cc_major_non_cardiac_surgery', 'cc_neutropenia_leukopenia',
      'cc_new_fluoroquinolone', 'cc_transverse_myelitis', 'cc_covid_female', 'cc_covid_male',
      'cc_covid_female_0_to_50', 'cc_covid_female_51_to_150', 'cc_covid_male_0_to_50',
      'cc_covid_male_51_to_150')
  )
  resDomain <- benchmarkCohortConstructor(cdm, runCohortConstructorDefinition = FALSE, dropCohorts = FALSE)
  expect_equal(
    settings(resDomain) |> dplyr::pull("result_type") |> unique() |> sort(),
    c('cohort_attrition', 'cohort_count', 'instanciation_time', 'omop_counts', 'summarise_cohort_overlap', 'summarise_cohort_timing')
  )
  expect_equal(
    resDomain |>
      omopgenerics::filterSettings(result_type == "summarise_cohort_overlap") |>
      dplyr::pull("group_level") |>
      unique() |>
      sort(),
    c(
      'atlas_asthma_no_copd &&& cc_asthma_no_copd',
      'atlas_beta_blockers_hypertension &&& cc_beta_blockers_hypertension',
      'atlas_covid &&& cc_covid', 'atlas_covid_female &&& cc_covid_female',
      'atlas_covid_female_0_to_50 &&& cc_covid_female_0_to_50',
      'atlas_covid_female_51_to_150 &&& cc_covid_female_51_to_150',
      'atlas_covid_male &&& cc_covid_male', 'atlas_covid_male_0_to_50 &&& cc_covid_male_0_to_50',
      'atlas_covid_male_51_to_150 &&& cc_covid_male_51_to_150',
      'atlas_endometriosis_procedure &&& cc_endometriosis_procedure',
      'atlas_hospitalisation &&& cc_hospitalisation',
      'atlas_major_non_cardiac_surgery &&& cc_major_non_cardiac_surgery',
      'atlas_neutropenia_leukopenia &&& cc_neutropenia_leukopenia',
      'atlas_new_fluoroquinolone &&& cc_new_fluoroquinolone',
      'atlas_transverse_myelitis &&& cc_transverse_myelitis'
    )
  )
  expect_true(
    any(omopgenerics::listSourceTables(cdm) == "benchmark")
  )
})
