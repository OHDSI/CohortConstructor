#' Assert if an object is a character and fulfill certain conditions.
#'
#' @param x To check.
#' @param length Length that has to have.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be null.
#' @param named Whether it has to be named.
#' @param minNumCharacter Minimum number of characters.
#' @param call Call argument that will be passed to `cli`.
#'
#' @noRd
#'
assertCharacter <- function(x,
                            length = NULL,
                            na = FALSE,
                            null = FALSE,
                            named = FALSE,
                            minNumCharacter = 0,
                            call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a character",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    ifelse(
      minNumCharacter > 0,
      paste("; at least", minNumCharacter, "per element"),
      ""
    ),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!is.character(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # minimum number of characters
    if (any(nchar(xNoNa) < minNumCharacter)) {
      cli::cli_abort(errorMessage, call = call)
    }
  }

  return(invisible(x))
}

#' Assert if an object is a list and fulfill certain conditions.
#'
#' @param x To check.
#' @param length Length that has to have.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be null.
#' @param named Whether it has to be named.
#' @param class Class that elements must have.
#' @param call Call argument that will be passed to `cli`.
#'
#' @noRd
#'
assertList <- function(x,
                       length = NULL,
                       na = FALSE,
                       null = FALSE,
                       named = FALSE,
                       class = NULL,
                       call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a list",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    ifelse(
      !is.null(class),
      paste("; elements must have class:", paste0(class, collapse = ", ")),
      ""
    ),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!is.list(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # assert class
    if (!is.null(class)) {
      flag <- lapply(xNoNa, function(y) {
        any(class %in% base::class(y))
      }) |>
        unlist() |>
        all()
      if (flag != TRUE) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}

#' Assert if an object is a choice and fulfill certain conditions.
#'
#' @param x To check.
#' @param choices Options that x can be.
#' @param length Length that has to have.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be null.
#' @param named Whether it has to be named.
#' @param call Call argument that will be passed to `cli`.
#'
#' @noRd
#'
assertChoice <- function(x,
                         choices,
                         length = NULL,
                         na = FALSE,
                         null = FALSE,
                         named = FALSE,
                         call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a choice between: ",
    paste0(choices, collapse = ", "),
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!all(class(x) == class(choices))) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # assert choices
    if (base::length(xNoNa) > 0) {
      if (!all(xNoNa %in% choices)) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}

#' Assert if an object is a logical and fulfill certain conditions.
#'
#' @param x To check.
#' @param length Length that has to have.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be null.
#' @param named Whether it has to be named.
#' @param call Call argument that will be passed to `cli`.
#'
#' @noRd
#'
assertLogical <- function(x,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a logical",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!is.logical(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)
  }

  return(invisible(x))
}

#' Assert if an object is a numeric and fulfill certain conditions.
#'
#' @param x To check.
#' @param integerish Whether elements must be integerish.
#' @param min Lower bound.
#' @param max Upper bound.
#' @param length Length that has to have.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be null.
#' @param named Whether it has to be named.
#' @param call Call argument that will be passed to `cli`.
#'
#' @noRd
#'
assertNumeric <- function(x,
                          integerish = FALSE,
                          min = -Inf,
                          max = Inf,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a numeric",
    ifelse(integerish, "; it has to be integerish", ""),
    ifelse(is.infinite(min), "", paste0("; equal or greater than ", min)),
    ifelse(is.infinite(max), "", paste0("; smaller than ", max)),
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!is.numeric(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert integerish
    if (integerish && base::length(xNoNa) > 0) {
      err <- max(abs(xNoNa - round(xNoNa)))
      if (err > 0.0001) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert lower bound
    if (!is.infinite(min) && base::length(xNoNa) > 0) {
      if (base::min(xNoNa) < min) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert upper bound
    if (!is.infinite(max) && base::length(xNoNa) > 0) {
      if (base::max(xNoNa) > max) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)
  }

  return(invisible(x))
}


assertLength <- function(x, length, errorMessage, call) {
  if (!is.null(length) && base::length(x) != length) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}

assertNa <- function(x, na, errorMessage, call) {
  if (!na && any(is.na(x))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}

assertNamed <- function(x, named, errorMessage, call) {
  if (named && length(names(x)[names(x) != ""]) != length(x)) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}

assertNull <- function(x, null, errorMessage, call) {
  if (!null && is.null(x)) {
    cli::cli_abort(errorMessage, call = call)
  }
  return(!is.null(x))
}

#' Assert that an object is a Date.
#'
#' @param x To check.
#' @param length Length that has to have.
#' @param call Call argument that will be passed to `cli`.
#'
#' @noRd
#'
assertDate <- function(x, length, call = parent.frame()) {
  # create error message
  errorMessage <- paste0(substitute(x), " must be an object of class Date.")
  if (!class(x) %in% "Date") {
    cli::cli_abort(errorMessage, call = call)
  }
  errorMessage <- paste0(substitute(x), " must have length = ", length)
  if (length(x) != length) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}

# Error messages ----
errorLength <- function(length) {
  if (!is.null(length)) {
    str <- paste0("; with length = ", length)
  } else {
    str <- ""
  }
  return(str)
}

errorNa <- function(na) {
  if (na) {
    str <- ""
  } else {
    str <- "; it can not contain NA"
  }
  return(str)
}

errorNamed <- function(named) {
  if (named) {
    str <- "; it has to be named"
  } else {
    str <- ""
  }
  return(str)
}

errorNull <- function(null) {
  if (null) {
    str <- ""
  } else {
    str <- "; it can not be NULL"
  }
  return(str)
}
