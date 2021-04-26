#' @inherit check_type
#'
#' @title Check If Argument Is Single Character
#'
#' @description Check if an argument is a single character. and if not,
#' generate an error message.
#'
#' Can be used to check argument names, for example.
#'
#' @param specific Optional. A single character which gives a detailed
#' description of the error. By default, this is generated automatically.
#' Can be turned off with `character(0)`.
#'
#' @seealso "Examples" section in [check_type()] for how to customize
#' error message and how to add and retrieve additional arguments.
#'
#' `vignette("erify")` for a gentle introduction to this package.
#'
#' @export
#'
#' @examples
#' x <- "a"
#' check_string(x)
#'
#' \dontrun{
#' # `x` must have type character
#' x <- c
#' check_string(x)
#'
#' # `x` must have length 1
#' x <- c("a", "b")
#' check_string(x)
#'
#' # `NA_character_` is not acceptable
#' x <- NA_character_
#' check_string(x)
#' }
check_string <- function(x, name = NULL, general = NULL, specific = NULL,
                         supplement = NULL, ...) {
  check_arguments(name, general, specific, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  .check_string(x, name, general, specific, supplement, ...)
}


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

  valid <-
    ifelse(zero, "x >= 0", "x > 0") %>%
    paste("&& is_integer(x)")

  .check_content(x, valid, name, general, specific, supplement, ...)
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
