source(here::here("data-raw", "domainsData.R"))
source(here::here("data-raw", "getBenchmarkResults.R"))
usethis::use_data(domainsData, benchmarkData, internal = TRUE, overwrite = TRUE)
