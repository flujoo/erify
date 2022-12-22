#' @inherit check_types
#'
#' @title Check If Each Item Is in Interval
#'
#' @description Check if each item of an argument is a number in an interval,
#' and if not, generate an error message.
#'
#' @param x The argument to check.
#'
#' @param valid A numeric vector of length 2, which represents the valid
#' closed interval. If `valid` is an integer vector,
#' `x` must also be an integer. `valid` can contain `NA`. For example,
#' `c(1, NA)` means `x` must be no less than 1.
#'
#' @export
#'
#' @examples
#' x <- c(1, 3, 5)
#' check_intervals(x, c(0, 6))
#'
#' \dontrun{
#' check_intervals(x, c(2, 4))
#' }
check_intervals <- function(x,
                            valid,
                            name = NULL,
                            general = NULL,
                            specific = NULL,
                            supplement = NULL,
                            ...) {
  if (is.null(name)) name <- deparse(substitute(x))
  if (is.null(general)) general <- phrase_valid_interval(valid, name, TRUE)

  types <- c("double", "integer")
  check_type(x, types, name, general, specific, supplement, ...)

  valid <- normalize_interval_valid(valid, TRUE)
  check_contents(x, valid, name, general, specific, supplement, TRUE, ...)
}
