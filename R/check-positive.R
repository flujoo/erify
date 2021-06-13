#' @inherit check_string
#'
#' @title Check If Argument Is Single Positive Number
#'
#' @description Check if an argument is a single positive number,
#' and if not, generate an error message.
#'
#' @param zero Optional. `TRUE` or `FALSE` which indicates if zero is
#' acceptable. The default value is `FALSE`.
#'
#' @export
#'
#' @examples
#' x <- 1.1
#' check_positive(x)
#'
#' x <- 1L
#' check_positive(x)
#'
#' \dontrun{
#' # `x` must be a numeric
#' x <- "1"
#' check_positive(x)
#'
#' # `x` must have length 1
#' x <- 1:2
#' check_positive(x)
#'
#' # `x` must not be `NA`
#' x <- NA_integer_
#' check_positive(x)
#'
#' # `x` must be larger than 0
#' x <- -1
#' check_positive(x)
#'
#' # make `0` acceptable
#' x <- 0
#' check_positive(x)
#' check_positive(x, zero = TRUE)
#' }
check_positive <- function(x, name = NULL, general = NULL, specific = NULL,
                           supplement = NULL, zero = FALSE, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(general)) {
    if (zero) {
      general <- "`{name}` must be a single non-negative number."
    } else {
      general <- "`{name}` must be a single positive number."
    }
  }

  check_type(
    x, c("double", "integer"), name, general, specific, supplement, ...)

  check_length(x, 1, name, general, specific, supplement, ...)

  valid <- paste(
    "is.finite(x) &",
    ifelse(zero, "x >= 0", "x > 0")
  )

  check_content(x, valid, name, general, specific, supplement, ...)
}
