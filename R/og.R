
# This file will be deleted when we depend on new omopgenerics

#' @export
#' @importFrom dplyr select
select.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
  keepClass(.data) |>
    dplyr::select(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

#' @export
#' @importFrom dplyr rename
rename.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
 keepClass(.data) |>
    dplyr::rename(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

#' @export
#' @importFrom dplyr distinct
distinct.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
  keepClass(.data) |>
    dplyr::distinct(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

#' @export
#' @importFrom dplyr slice
slice.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
  keepClass(.data) |>
    dplyr::slice(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

#' @export
#' @importFrom dplyr slice_head
slice_head.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
  keepClass(.data) |>
    dplyr::slice_head(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

#' @export
#' @importFrom dplyr slice_tail
slice_tail.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
  keepClass(.data) |>
    dplyr::slice_tail(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

#' @export
#' @importFrom dplyr slice_min
slice_min.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
  keepClass(.data) |>
    dplyr::slice_min(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

#' @export
#' @importFrom dplyr slice_max
slice_max.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
  keepClass(.data) |>
    dplyr::slice_max(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

#' @export
#' @importFrom dplyr slice_sample
slice_sample.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  at <- keepAttributes(.data, cl)
  keepClass(.data) |>
    dplyr::slice_sample(...) |>
    restoreClass(cl) |>
    restoreAttributes(at)
}

keepAttributes <- function(x, cl) {
  xx <- list(
    tbl_source = attr(x, "tbl_source"),
    tbl_name = attr(x, "tbl_name"),
    cdm_reference = attr(x, "cdm_reference")
  )
  if ("cohort_table" %in% cl) {
    xx[["cohort_set"]] <- attr(x, "cohort_set")
    xx[["cohort_attrition"]] <- attr(x, "cohort_attrition")
    xx[["cohort_codelist"]] <- attr(x, "cohort_codelist")
  }
  return(xx)
}
keepClass <- function(x) {
  omopgenerics:::removeClass(x = x, value = c(
    "cdm_table", "omop_table", "achilles_table", "cohort_table",
    "summarised_result"
  ))
}
restoreAttributes <- function(x, at) {
  for (nm in names(at)) {
    if (!nm %in% names(attributes(x))) {
      attr(x, nm) <- at[[nm]]
    }
  }
  return(x)
}
restoreClass <- function(x, cl) {
  x <- omopgenerics:::addClass(x, "cdm_table")
  if ("cohort_table" %in% cl &
      "cohort_definition_id" %in% colnames(x)) {
    x <- omopgenerics:::addClass(x, "cohort_table")
  }
  return(x)
}
