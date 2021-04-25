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
#' @title Check If Argument Is Single Positive Integer
#'
#' @description Check if an argument is a single positive integer,
#' and if not, generate an error message.
#'
#' Can be used to check indices, for example.
#'
#' The term "integer" is used in a mathematical sense, which means `1` and
#' `1L` are both integers.
#'
#' @export
#'
#' @examples
#' x <- 1
#' check_index(x)
#'
#' x <- 1L
#' check_index(x)
#'
#' \dontrun{
#' # `x` must be a numeric
#' x <- "1"
#' check_index(x)
#'
#' # `x` must have length 1
#' x <- 1:2
#' check_index(x)
#'
#' # `x` must not be `NA`
#' x <- NA_integer_
#' check_index(x)
#'
#' # `x` must be larger than 0
#' x <- -1
#' check_index(x)
#'
#' # `x` must be an integer in a mathematical sense
#' x <- 1.1
#' check_index(x)
#' }
check_index <- function(x, name = NULL, general = NULL, specific = NULL,
                        supplement = NULL, ...) {
  check_arguments(name, general, specific, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(general)) {
    general <- "`{name}` must be a single positive integer."
  }

  .check_type(
    x, c("double", "integer"), name, general, specific, supplement, ...)
  .check_length(x, 1, NULL, name, general, specific, supplement, ...)
  .check_content(
    x, "is_integer(x) && x > 0", name, general, specific, supplement, ...)
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
