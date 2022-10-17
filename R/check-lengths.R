#' @inherit check_length
#'
#' @title Check Each Item's Length
#'
#' @description Check if each item of an argument has valid length,
#' and if not, generate an error message.
#'
#' @param x The argument to check, which must be a list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- list(1, c(1, 2), c(1, 2, 3))
#'
#' check_lengths(x, c(1, NA))
#'
#' specific = "Item {i} has length {feature}."
#' check_lengths(x, c(1, NA), specific = specific)
#' }
check_lengths <- function(x, valid, name = NULL, general = NULL,
                          specific = NULL, supplement = NULL,
                          interval = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(specific)) {
    specific <- "`{name}[[{i}]]` has length {feature}."
  }

  # normalize `interval`
  if (is.null(interval)) {
    interval <- normalize_length_interval(interval, valid)
  }

  specifics <- character(0)

  for (i in seq_along(x)) {
    feature <- length(x[[i]])
    pass <- is_valid_length(feature, valid, interval)

    if (!pass) {
      specifics <- c(specifics, glue::glue(specific))
    }
  }

  if (is.null(general)) {
    s_valid <- phrase_valid_length(valid, interval)
    general <- "Each item of `{name}` must have length {s_valid}."
  }

  throw(general, specifics, environment(), ...)
}
