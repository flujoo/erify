#' @inherit check_string
#'
#' @title Check Argument's Content
#'
#' @description Check if an argument is from some given choices or satisfies
#' some requirement, and if not, generate an error message.
#'
#' @param valid can be
#'
#' 1. a function, which takes `x` as argument and returns `TRUE` or `FALSE`,
#' 2. an expression, which contains `x` and evaluates to `TRUE` or `FALSE`,
#' 3. a string of R code, which evaluates to `TRUE` or `FALSE`, or
#' 4. a non-empty atomic vector, which contains the valid choices.
#'
#' @param as_double Optional. `TRUE` or `FALSE` which indicates if to
#' differentiate between type double and integer. The default value is
#' `TRUE`, which means integers are handled as doubles.
#'
#' @export
#'
#' @examples
#' valid <- c(1, 2, 3)
#'
#' x <- 2L
#' check_content(x, valid)
#'
#' \dontrun{
#' # `x` must have the same type with `valid`
#' x <- "a"
#' check_content(x, valid)
#'
#' # `x` must have length 1
#' x <- c(1, 2)
#' check_content(x, valid)
#'
#' # differentiate between type double and integer
#' x <- 2L
#' check_content(x, valid, as_double = FALSE)
#'
#' # `valid` can be a function
#' check_content(x, is.na, general = "`x` must be `NA`.")
#'
#' # `valid` can be a string of R code
#' check_content(x, "is.na(x)", general = "`x` must be `NA`.")
#' }
check_content <- function(x, valid, name = NULL, general = NULL,
                          specific = NULL, supplement = NULL,
                          as_double = TRUE, ...) {
  if (is.null(name)) name <- deparse(substitute(x))
  check_length(x, 1, name, general, specific, supplement, ...)

  if (is.function(valid)) {
    pass <- valid(x)

  } else if (is.expression(valid)) {
    pass <- eval(valid)

  } else if (is.character(valid) && length(valid) == 1) {
    pass <- eval(parse(text = valid))

  } else {
    # get valid types
    if (as_double && is.numeric(valid)) {
      types <- c("double", "integer")
    } else {
      types <- typeof(valid)
    }

    check_type(x, types, name, general, specific, supplement, ...)

    pass <- x %in% valid
  }

  if (pass) {
    return(invisible(NULL))
  }

  if (is.null(general)) {
    s_valid <- phrase_valid_content(valid, as_double)

    if (!is.null(s_valid)) {
      general <- "`{name}` must be {s_valid}."
    }
  }

  if (is.null(specific)) {
    specific <- "`{name}` is { back_quote(x, as_double = as_double) }."

  } else if (length(specific) == 0) {
    # turn off `specific` with empty vector
    specific <- NULL
  }

  specifics <- c(specific, supplement)
  throw(general, specifics, environment(), ...)
}


phrase_valid_content <- function(valid, as_double) {
  if (as_double && is.integer(valid)) {
    valid <- as.double(valid)
  }

  if (is.atomic(valid)) {
    join(back_quote(valid, as_double = as_double))
  }
}
