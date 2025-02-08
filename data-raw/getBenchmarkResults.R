library(readr)
library(omopgenerics)
library(here)
library(dplyr)
library(tidyr)
library(visOmopResults)
library(stringr)

readData <- function(path) {
  zipFiles <- list.files(path = path, pattern = ".zip")
  tempfolder <- tempdir()
  data <- NULL
  for (file in zipFiles) {
    file <- file.path(path, file)
    fname <- unzip(file, list = TRUE)$Name
    fname <- fname[tools::file_ext(fname) == "csv"]
    unzip(file, files = fname, exdir = tempfolder, overwrite = TRUE)
    files <- file.path(tempfolder, fname)
    data <- c(data, readFiles(files))
  }
  return(data)
}

readFiles <- function(files) {
  data <- list()
  for (file in files) {
    data[[file]] <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()))
    if (all(colnames(data[[file]]) %in% omopgenerics::resultColumns()) & "settings" %in% data[[file]]$variable_name) {
      data[[file]] <- data[[file]] |> omopgenerics::newSummarisedResult()
    }
  }
  names(data) <- basename(tools::file_path_sans_ext(names(data)))
  return(data)
}

mergeData <- function(data, patterns) {
  x <- list()
  for (pat in patterns) {
    dataSubset <- data[grepl(pat, names(data))]
    srExp <- length(dataSubset)
    srObs <- sum(lapply(data[grepl(pat, names(data))], class) |> unlist() == "summarised_result")
    if (srObs > 0) {
      if (srObs == srExp) {
        for(i in seq_along(dataSubset)) {
          if(!is.null(attr(dataSubset[[i]], "settings"))){
          attr(dataSubset[[i]], "settings") <-  attr(dataSubset[[i]], "settings") |>
              mutate(across(.cols = -result_id, as.character))
          }
        }
        x[[pat]] <- dataSubset |> omopgenerics::bind()
      } else {
        cli::cli_abort("Not all results with pattern {pat} have class summarised result.")
      }
    }  else {
      x[[pat]] <- dataSubset |> dplyr::bind_rows() |> distinct()
    }
  }
  return(x)
}

updateCDMname <- function(resultList, old, new) {
  caseWhen <- "dplyr::case_when("
  for (k in seq_along(old)) {
    caseWhen <- glue::glue("{caseWhen} .data$cdm_name == '{old[k]}' ~ '{new[k]}', ")
  }
  caseWhen <- paste0(caseWhen, ".default = .data$cdm_name)") |>
    rlang::parse_exprs() |> rlang::set_names("cdm_name")
  for (res in names(resultList)) {
    resultList[[res]] <- resultList[[res]] |>
      dplyr::mutate(cdm_name = !!!caseWhen)
  }
  resultList
}

updateResultType <- function(resultList, old, new) {
  caseWhen <- "dplyr::case_when("
  for (k in seq_along(old)) {
    caseWhen <- glue::glue("{caseWhen} .data$result_type == '{old[k]}' ~ '{new[k]}', ")
  }
  caseWhen <- paste0(caseWhen, ".default = .data$result_type)") |>
    rlang::parse_exprs() |> rlang::set_names("result_type")
  for (res in names(resultList)) {
    if (inherits(resultList[[res]], "summarised_result")) {
      attr(resultList[[res]], "settings") <- settings(resultList[[res]]) |>
        dplyr::mutate(cdm_name = !!!caseWhen)
    }
  }
  resultList
}

# Functions
niceNum <- function(x, dec = 0) {
  trimws(format(round(as.numeric(x), dec), big.mark = ",", nsmall = dec, scientific = FALSE))
}

# get data ----
resultPatterns <- c("time", "comparison", "details", "omop", "index_counts", "sql_indexes")
benchmarkDataPre <- readData(here::here("data-raw", "benchmarkResults")) |>
  mergeData(resultPatterns) |>
  updateCDMname(old = "AurumCDM_202403", new = "CPRD Aurum") |>
  updateResultType(old = "cohort_overlap", new = "summarise_cohort_overlap")
benchmarkData <- list()

### omop
benchmarkData$omop <- benchmarkDataPre$omop |>
  filter(table_name != "death") |>
  select("cdm_name", "OMOP table" = "table_name", "number_records") |>
  mutate(
    number_records = niceNum(number_records, 0),
    "OMOP table" = factor(
      `OMOP table`,
      levels = c("person", "observation_period", "drug_exposure", "condition_occurrence",
                 "procedure_occurrence", "visit_occurrence", "measurement", "observation")
    )
  ) |>
  arrange(.data$`OMOP table`) |>
  pivot_wider(names_from = c("cdm_name"), values_from = c("number_records"), names_prefix = "[header]Database\n[header_level]")

### details
benchmarkData$details <- benchmarkDataPre$details |>
  filterSettings(result_type == "cohort_count") |>
  formatEstimateValue() |>
  splitAll() |>
  select(!"estimate_type") |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  select(-variable_level, - result_id) |>
  distinct() |>
  filter(grepl("cc_|atlas_", cohort_name)) |>
  mutate(
    Tool = case_when(grepl("cc", cohort_name) ~ "CohortConstructor", grepl("atlas", cohort_name) ~ "CIRCE", .default = NA),
    "Cohort name" = str_to_sentence(gsub("_", " ", gsub("cc_|atlas_", "", cohort_name))),
    variable_name = stringr::str_to_sentence(gsub("_", " ", .data$variable_name)),
    "Cohort name" = case_when(
      grepl("Asthma", .data[["Cohort name"]]) ~ "Asthma without COPD",
      grepl("Covid", .data[["Cohort name"]]) ~ gsub("Covid|Covid", "COVID-19", `Cohort name`),
      grepl("eutropenia", .data[["Cohort name"]]) ~ "Acquired neutropenia or unspecified leukopenia",
      grepl("Hosp", .data[["Cohort name"]]) ~ "Inpatient hospitalisation",
      grepl("First", .data[["Cohort name"]]) ~ "First major depression",
      grepl("fluoro", .data[["Cohort name"]]) ~ "New fluoroquinolone users",
      grepl("Beta", .data[["Cohort name"]]) ~ "New users of beta blockers nested in essential hypertension",
      .default = .data[["Cohort name"]]
    ),
    "Cohort name" = if_else(
      grepl("COVID", .data[["Cohort name"]]),
      gsub(" female", ": female", gsub(" male", ": male", .data[["Cohort name"]])),
      .data[["Cohort name"]]
    ),
    "Cohort name" = if_else(
      grepl(" to ", .data[["Cohort name"]]),
      gsub("male ", "male, ", .data[["Cohort name"]]),
      .data[["Cohort name"]]
    )
  ) |>
  arrange(`Cohort name`) |>
  select(-cohort_name) |>
  pivot_wider(names_from = c("Tool", "variable_name"), values_from = c("count"), names_prefix = "[header]Tool\n[header_level]", names_sep = "\n[header_level]")

### time definition
benchmarkData$time_definition <- benchmarkDataPre$time |>
  distinct() |>
  filter(!grepl("male|set", msg)) |>
  mutate(
    time = (as.numeric(toc) - as.numeric(tic))/60,
    Tool = if_else(grepl("cc", msg), "CohortConstructor", "CIRCE"),
    "Cohort name" = str_to_sentence(gsub("_", " ", gsub("cc_|atlas_", "", msg)))
  ) |>
  select(-c("tic", "toc", "msg", "callback_msg")) |>
  mutate(
    "Cohort name" = case_when(
      grepl("Asthma", .data[["Cohort name"]]) ~ "Asthma without COPD",
      grepl("Covid", .data[["Cohort name"]]) ~ "COVID-19",
      grepl("eutropenia", .data[["Cohort name"]]) ~ "Acquired neutropenia or unspecified leukopenia",
      grepl("Hosp", .data[["Cohort name"]]) ~ "Inpatient hospitalisation",
      grepl("First", .data[["Cohort name"]]) ~ "First major depression",
      grepl("fluoro", .data[["Cohort name"]]) ~ "New fluoroquinolone users",
      grepl("Beta", .data[["Cohort name"]]) ~ "New users of beta blockers nested in essential hypertension",
      .default = .data[["Cohort name"]]
    )
  ) |>
  arrange(`Cohort name`)

### time domain
header_prefix <- "[header]Time (minutes)\n[header_level]"
benchmarkData$time_domain <- benchmarkDataPre$time |>
  distinct() |>
  filter(grepl("atlas", msg)) |>
  filter(!grepl("male", msg)) |>
  group_by(cdm_name) |>
  summarise(time = niceNum(sum(as.numeric(toc) - as.numeric(tic))/60, 2)) |>
  mutate(Tool = "CIRCE") |>
  union_all(
    benchmarkDataPre$time |>
      filter(msg == "cc_set_no_strata") |>
      group_by(cdm_name) |>
      summarise(time = niceNum(sum(as.numeric(toc) - as.numeric(tic))/60, 2)) |>
      mutate(Tool = "CohortConstructor")
  ) |>
  pivot_wider(names_from = "Tool", values_from = "time", names_prefix = header_prefix) |>
  select(c("Database_name" = "cdm_name", starts_with(header_prefix)))

### time strata
benchmarkData$time_strata <- benchmarkDataPre$time |>
  mutate(Tool = if_else(grepl("cc", msg), "CohortConstructor", "CIRCE")) |>
  group_by(cdm_name, Tool) |>
  summarise(time = niceNum(sum(as.numeric(toc) - as.numeric(tic))/60, 2), .groups = "drop") |>
  pivot_wider(names_from = "Tool", values_from = "time", names_prefix = "[header]Time (minutes)\n[header_level]") |>
  select("Database" = "cdm_name", starts_with("[header]Time"))

### time comparison
benchmarkData$comparison <- benchmarkDataPre$comparison |>
  filterSettings(result_type == "summarise_cohort_overlap") |>
  splitGroup() |>
  filter(grepl("atlas_", cohort_name_comparator) & grepl("cc_", cohort_name_reference)) |>
  filter(gsub("atlas_", "", cohort_name_comparator) == gsub("cc_", "", cohort_name_reference)) |>
  uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator")) |>
  mutate(
    variable_name = case_when(
      .data$variable_name == "overlap" ~ "In both cohorts",
      .data$variable_name == "reference" ~ "Only in reference cohort",
      .data$variable_name == "comparator" ~ "Only in comparator cohort",
      .default = .data$variable_name
    ),
    variable_level = "Subjects"
  ) |>
  newSummarisedResult()

### sql indexes
benchmarkData$sql_indexes <- benchmarkDataPre$sql_indexes


usethis::use_data(benchmarkData, internal = FALSE, overwrite = TRUE)
