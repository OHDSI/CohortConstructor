library(readr)
library(omopgenerics)
library(here)
library(dplyr)
library(tidyr)

readData <- function(path) {
  zipFiles <- list.files(path = path, pattern = ".zip")
  tempfolder <- tempdir()
  data <- NULL
  for (file in zipFiles) {
    file <- file.path(path, file)
    fname = unzip(file, list = TRUE)$Name
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
        x[[pat]] <- dataSubset %>% omopgenerics::bind()
      } else {
        cli::cli_abort("Not all results with pattern {pat} have class summarised result.")
      }
    }  else {
      x[[pat]] <- dataSubset %>% dplyr::bind_rows() %>% distinct()
    }
  }
  return(x)
}

result_patterns <- c("time", "comparison", "details", "omop", "index_counts", "sql_indexes")
data <- readData(here("extras", "data")) %>% mergeData(result_patterns)
save(data, file = here("extras", "benchmark.RData"))
