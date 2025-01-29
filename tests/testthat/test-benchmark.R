test_that("benchmark works", {
  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema   = "main", writeSchema = "main")

})
