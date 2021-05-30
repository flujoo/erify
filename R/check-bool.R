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
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  check_content(x, c(TRUE, FALSE), name, general, specific, supplement, ...)
}
