#' @inherit check_type
#'
#' @title Check If Argument Is in Interval
#'
#' @description Check if an argument is a number in an interval, and if not,
#' generate an error message.
#'
#' @param valid A numeric vector of length 2, which represents the valid
#' closed interval. If `valid` is an integer vector,
#' `x` must also be an integer. `valid` can contain `NA`. For example,
#' `c(1, NA)` means `x` must be no less than 1.
#'
#' @export
#'
#' @examples
#' x <- 3.3
#'
#' check_interval(x, c(1, 5))
#'
#' \dontrun{
#' check_interval(x, c(1L, 5L))
#' check_interval(x, c(4, NA))
#' check_interval(x, c(NA, 2))
#' }
check_interval <- function(x,
                           valid,
                           name = NULL,
                           general = NULL,
                           specific = NULL,
                           supplement = NULL,
                           ...) {
  if (is.null(name)) name <- deparse(substitute(x))
  if (is.null(general)) general <- phrase_valid_interval(valid, name)

  types <- c("double", "integer")
  check_type(x, types, name, general, specific, supplement, ...)
  check_length(x, 1, name, general, specific, supplement, NULL, ...)

  valid <- normalize_interval_valid(valid)
  check_content(x, valid, name, general, specific, supplement, TRUE, ...)
}


phrase_valid_interval <- function(valid, name) {
  type <- if (is.integer(valid)) "an integer" else "a number"

  valid_1 <- valid[1]
  valid_2 <- valid[2]

  if (is.na(valid_1)) {
    phrase <- paste("no larger than", valid_2)
  } else if (is.na(valid_2)) {
    phrase <- paste("no less than", valid_1)
  } else {
    phrase <- paste("between", valid_1, "and", valid_2)
  }

  paste0("`", name, "`", " must be ", type, " ", phrase, ".")
}


normalize_interval_valid <- function(valid) {
  valid_1 <- valid[1]
  valid_2 <- valid[2]

  normalized <- "!is.na(x)"

  if (is.integer(valid)) {
    normalized <- paste(normalized, "&& as.integer(x) == x")
  }

  if (!is.na(valid_2)) normalized <- paste(normalized, "&& x <=", valid_2)
  if (!is.na(valid_1)) normalized <- paste(normalized, "&& x >=", valid_1)

  normalized
}
