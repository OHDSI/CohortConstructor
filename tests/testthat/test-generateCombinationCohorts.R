test_that("gteIdentifier", {
  x <- dplyr::tibble(a = 1)

  expect_no_error(id <- getIdentifier(x))

  expect_true(getIdentifier(x, len = 5) |> length() == 5)
  expect_true(getIdentifier(x, nchar = 2) |> nchar() == 2)
  expect_true(getIdentifier(x, prefix = "id_", nchar = 2) |> nchar() == 5)
  expect_true(getIdentifier(x, prefix = "id_", nchar = 8) |> substr(1, 3) == "id_")
})

test_that("joinOverlap", {
  x <- dplyr::tibble(
    start_date = as.Date(c(
      "2020-01-01", "2020-03-01", "2020-06-01", "2020-02-01", "2020-05-02",
      "2020-03-01", "2020-06-01", "2020-04-01"
    )),
    end_date = as.Date(c(
      "2020-04-01", "2020-06-01", "2020-08-01", "2020-05-01", "2020-07-01",
      "2020-05-01", "2020-08-01", "2020-07-01"
    )),
    pid = c(1, 1, 1, 1, 1, 2, 2, 2),
    def_id = c(1, 1, 1, 2, 2, 1, 1, 2)
  )
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(db, "x", x)
  x <- dplyr::tbl(db, "x")

  # gap = 0
  expect_no_error(
    res <- joinOverlap(
      x, start = "start_date", end = "end_date", by = c("pid", "def_id")
    ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  )
  expect_true(nrow(res) == 6)
  expect_identical(
    res,
    dplyr::tibble(
      pid = c(1, 1, 1, 2, 2, 2),
      def_id = c(1, 2, 2, 1, 1, 2),
      start_date = as.Date(c(
        "2020-01-01", "2020-02-01", "2020-05-02", "2020-03-01", "2020-06-01",
        "2020-04-01"
      )),
      end_date = as.Date(c(
        "2020-08-01", "2020-05-01", "2020-07-01", "2020-05-01", "2020-08-01",
        "2020-07-01"
      ))
    )
  )

  # gap = 1
  expect_no_error(
    res <- joinOverlap(
      x, start = "start_date", end = "end_date", by = c("pid", "def_id"), gap = 1
    ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  )
  expect_true(nrow(res) == 5)
  expect_identical(
    res,
    dplyr::tibble(
      pid = c(1, 1, 2, 2, 2),
      def_id = c(1, 2, 1, 1, 2),
      start_date = as.Date(c(
        "2020-01-01", "2020-02-01", "2020-03-01", "2020-06-01", "2020-04-01"
      )),
      end_date = as.Date(c(
        "2020-08-01", "2020-07-01", "2020-05-01", "2020-08-01", "2020-07-01"
      ))
    )
  )

  DBI::dbDisconnect(db, shutdown = TRUE)
})

test_that("splitOverlap", {
  x <- dplyr::tibble(
    start_date = as.Date(c(
      "2020-01-01", "2020-03-01", "2020-06-01", "2020-02-01", "2020-05-02",
      "2020-03-01", "2020-06-01", "2020-04-01"
    )),
    end_date = as.Date(c(
      "2020-04-01", "2020-06-01", "2020-08-01", "2020-05-01", "2020-07-01",
      "2020-05-01", "2020-08-01", "2020-07-01"
    )),
    pid = c(1, 1, 1, 1, 1, 2, 2, 2),
    def_id = c(1, 1, 1, 2, 2, 1, 2, 1)
  )
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(db, "x", x)
  x <- dplyr::tbl(db, "x")

  expect_no_error(
    res <- splitOverlap(
      x, start = "start_date", end = "end_date", by = c("pid", "def_id")
    ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  )
  expect_true(nrow(res) == 11)
  expect_identical(
    res,
    dplyr::tibble(
      pid = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
      def_id = c(1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2),
      start_date = as.Date(c(
        "2020-01-01", "2020-03-01", "2020-04-02", "2020-06-01", "2020-06-02",
        "2020-02-01", "2020-05-02", "2020-03-01", "2020-04-01", "2020-05-02",
        "2020-06-01"
      )),
      end_date = as.Date(c(
        "2020-08-01", "2020-05-01", "2020-07-01", "2020-05-01", "2020-08-01",
        "2020-07-01"
      ))
    )
  )

  DBI::dbDisconnect(db, shutdown = TRUE)
})
