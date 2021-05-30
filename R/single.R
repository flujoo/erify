#' @inherit check_string
#'
#' @title Check If Argument Is Single Natural Number
#'
#' @description Check if an argument is a single natural number,
#' and if not, generate an error message.
#'
#' Can be used to check indices, for example.
#'
#' @param zero Optional. `TRUE` or `FALSE` which indicates if zero is
#' acceptable. The default value is `FALSE`.
#'
#' @return `check_n()` returns an invisible `NULL` if the argument
#' is valid, or it generates an error message.
#'
#' `is_n()` returns `TRUE` or `FALSE`.
#'
#' @export
#'
#' @examples
#' x <- 1
#' check_n(x)
#'
#' x <- 1L
#' check_n(x)
#'
#' \dontrun{
#' # `x` must be a numeric
#' x <- "1"
#' check_n(x)
#'
#' # `x` must have length 1
#' x <- 1:2
#' check_n(x)
#'
#' # `x` must not be `NA`
#' x <- NA_integer_
#' check_n(x)
#'
#' # `x` must be larger than 0
#' x <- -1
#' check_n(x)
#'
#' # `x` must be an integer in a mathematical sense
#' x <- 1.1
#' check_n(x)
#'
#' # make `0` acceptable
#' x <- 0
#' check_n(x)
#' check_n(x, zero = TRUE)
#' }
check_n <- function(x, name = NULL, general = NULL, specific = NULL,
                        supplement = NULL, zero = NULL, ...) {
  # check arguments
  check_arguments(name, general, specific, supplement)

  if (!is.null(zero)) {
    check_bool(zero, general = getOption("erify.general"))
  }

  # capture name
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(zero)) {
    zero <- FALSE
  }

  if (is.null(general)) {
    if (zero) {
      general <- "`{name}` must be a single non-negative integer."
    } else {
      general <- "`{name}` must be a single positive integer."
    }
  }

  .check_type(
    x, c("double", "integer"), name, general, specific, supplement, ...)
  .check_length(x, 1, NULL, name, general, specific, supplement, ...)

  valid <- function(x) is_n(x, zero)
  .check_content(x, valid, name, general, specific, supplement, ...)
}


#' @rdname check_n
#' @export
is_n <- function(x, zero = NULL) {
  if (!is.null(zero)) {
    check_bool(zero)
  }

  if (is.null(zero)) {
    zero <- FALSE
  }

  is.numeric(x) &&
    is.finite(x) &&
    ifelse(zero, x >= 0, x > 0) &&
    (as.integer(x) == x)
}


#' @inherit check_string
#'
#' @title Check If Argument Is Single Logical
#'
#' @description Check if an argument is `TRUE` or `FALSE`, and if not,
#' generate an error message.
#'
#' @export
#'
#' @examples
#' x <- TRUE
#' check_bool(x)
#'
#' \dontrun{
#' # `x` must have type logical
#' x <- 1
#' check_bool(x)
#'
#' # `x` must have length 1
#' x <- c(TRUE, FALSE)
#' check_bool(x)
#'
#' # `x` must not be `NA`
#' x <- NA
#' check_bool(x)
#' }
check_bool <- function(x, name = NULL, general = NULL, specific = NULL,
                       supplement = NULL, ...) {
  check_arguments(name, general, specific, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  check_content(x, c(TRUE, FALSE), name, general, specific, supplement, ...)
}
