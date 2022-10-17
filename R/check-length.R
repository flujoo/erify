#' @inherit check_type
#'
#' @title Check Argument's Length
#'
#' @description Check if an argument has valid length, and if not,
#' generate an error message.
#'
#' @param valid A numeric vector which contains non-negative integers and
#' `NA`, used with argument `interval` to indicate the valid lengths.
#'
#' @param interval Optional. `TRUE` or `FALSE` which indicates if argument
#' `valid` is interpreted as an interval or as single lengths. For example,
#' `c(1, 10)` is interpreted as "larger than 1 and smaller than 10" if
#' `interval` is `TRUE`, but as "1 or 10" if `FALSE`. `NA` can be used in
#' `valid` when treated as interval. For example, `c(0, NA)` means "larger
#' than 0". By default, `interval` is inferred from `valid`. For example,
#' if `valid` has length unequal to 2, it's treated as single lengths.
#'
#' @seealso "Examples" section in [check_type()] for how to customize
#' error message and how to add and retrieve additional arguments.
#'
#' `vignette("erify")` for a gentle introduction to this package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(1, 2)
#'
#' # `valid` as interval
#' check_length(x, c(1, 3), interval = TRUE)
#' check_length(x, c(NA, 2))
#'
#' # `valid` as single lengths
#' check_length(x, c(1, 3), interval = FALSE)
#'
#' # customize error message with `glue::glue()` syntax
#' specific <- "Oh my god! `{name}`'s length is {feature}."
#' check_length(x, 3, specific = specific)
#' }
check_length <- function(x, valid, name = NULL, general = NULL,
                         specific = NULL, supplement = NULL, interval = NULL,
                         ...) {
  # normalize `interval`
  if (is.null(interval)) {
    interval <- normalize_length_interval(interval, valid)
  }

  feature <- length(x)
  pass <- is_valid_length(feature, valid, interval)

  if (pass) {
    return(invisible())
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(general)) {
    s_valid <- phrase_valid_length(valid, interval)
    general <- "`{name}` must have length {s_valid}."
  }

  if (is.null(specific)) {
    specific <- "`{name}` has length {feature}."

  } else if (length(specific) == 0) {
    # turn off `specific` with empty vector
    specific <- NULL
  }

  specifics <- c(specific, supplement)
  throw(general, specifics, environment(), ...)
}


normalize_length_interval <- function(interval, valid) {
  l <- length(valid)

  if (l != 2) {
    FALSE

  } else {
    con <- valid[2] > (valid[1] + 1)
    ifelse(is.na(con), TRUE, con)
  }
}


is_valid_length <- function(l, valid, interval) {
  if (!interval) {
    l %in% valid

  } else {
    con <- l > valid[1] && l < valid[2]
    ifelse(is.na(con), TRUE, con)
  }
}


phrase_valid_length <- function(valid, interval) {
  if (!interval) {
    join(valid)

  } else {
    v1 <- valid[1]
    v2 <- valid[2]
    ss <- character(0)

    if (!is.na(v1)) {
      ss <- c(ss, glue::glue("larger than {v1}"))
    }

    if (!is.na(v2)) {
      ss <- c(ss, glue::glue("smaller than {v2}"))
    }

    join(ss, "and")
  }
}
