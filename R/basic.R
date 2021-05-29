# class -------------------------------------------------------------------

#' @inherit check_length
#'
#' @title Check Argument's Class
#'
#' @description Check if an argument has valid class, and if not,
#' generate an error message.
#'
#' @param valid A character vector which contains valid classes.
#'
#' @export
#'
#' @examples
#' x <- 1
#' class(x) <- c("a", "b")
#'
#' check_class(x, c("a", "c"))
#'
#' \dontrun{
#' check_class(x, c("c", "d"))
#'
#' # customize error message with `glue::glue()` syntax
#' specific <- "Unbelievable! The first class of `{name}` is {feature[1]}."
#' check_class(x, c("c", "d"), specific = specific)
#' }
check_class <- function(x, valid, name = NULL, general = NULL,
                        specific = NULL, supplement = NULL, ...) {
  # check arguments
  check_type_valid(valid)
  check_arguments(name, general, specific, supplement)

  # validity
  pass <- inherits(x, valid)

  # early return
  if (pass) {
    return(invisible())
  }

  # capture name
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  # extract feature
  feature <- class(x)

  # general
  .general <- glue::glue("`{name}` must have class { .join(valid) }.")

  if (is.null(general)) {
    general <- .general
  }

  # specific
  if (is.null(specific)) {
    specific <- "`{name}` has class { .join(feature, 'and') }."
  }

  # add `supplement`
  specifics <- c(specific, supplement)

  .Statement(general, specifics, environment(), ...) %>% .trigger()
}



# content -----------------------------------------------------------------

#' @inherit check_string
#'
#' @title Check Argument's Content
#'
#' @description Check if an argument is from some given choices, and if not,
#' generate an error message.
#'
#' @param valid A non-empty atomic vector which contains the valid choices.
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
#' }
check_content <- function(x, valid, name = NULL, general = NULL,
                          specific = NULL, supplement = NULL,
                          as_double = NULL, ...) {
  # check arguments
  check_content_valid(valid)
  check_arguments(name, general, specific, supplement)

  if (!is.null(as_double)) {
    check_bool(as_double, general = getOption("erify.general"))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(as_double)) {
    as_double <- TRUE
  }

  con <- as_double && is.numeric(x) && is.numeric(valid)

  if (!con) {
    .check_type(x, typeof(valid), name, general, specific, supplement, ...)
  }

  .check_length(x, 1, NULL, name, general, specific, supplement, ...)
  .check_content(
    x, valid, name, general, specific, supplement, as_double,
    as_code = FALSE, ...
  )
}


check_content_valid <- function(valid) {
  # check type
  pass <- is.atomic(valid) && !is.null(valid)

  if (!pass) {
    pre <- getOption("erify.prepend")
    general <- paste(pre, "`valid` must have an atomic type.")
    specific <- "`valid` has type { typeof(valid) }."
    .Statement(general, specific, environment()) %>% .trigger()
  }

  # check length
  .check_length(valid, c(0, NA), general = getOption("erify.general"))
}



# length ------------------------------------------------------------------

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
#' check_length(x, c(1, 3), TRUE)
#' check_length(x, c(NA, 2))
#'
#' # `valid` as single lengths
#' check_length(x, c(1, 3), FALSE)
#'
#' # customize error message with `glue::glue()` syntax
#' specific <- "Oh my god! `{name}`'s length is {feature}."
#' check_length(x, 3, specific = specific)
#' }
check_length <- function(x, valid, interval = NULL, name = NULL,
                         general = NULL, specific = NULL,
                         supplement = NULL, ...) {
  # check arguments
  check_length_valid(valid, interval)

  if (!is.null(interval)) {
    check_bool(interval, general = getOption("erify.general"))
  }

  check_arguments(name, general, specific, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  .check_length(x, valid, interval, name, general, specific, supplement, ...)
}


check_length_valid <- function(valid, interval) {
  g <- getOption("erify.general")
  pre <- getOption("erify.prepend")

  # check type and length
  .check_type(valid, c("integer", "double"), general = g)
  .check_length(valid, c(0, NA), general = g)

  # check contents
  general <- paste(
    pre,
    "`valid` must contain only non-negative integers and `NA`."
  )

  .valid <- "is.na(x_i) || is_n(x_i, TRUE)"
  .check_contents(valid, .valid, general = general)

  #
  general <- paste(pre, "`valid` must contain at least one integer.")
  .check_content(valid, "!all(is.na(x))", NULL, general)

  #
  if (isTRUE(interval)) {
    general <- paste(
      pre,
      "If `interval` is `TRUE`, `valid` must have length 2."
    )

    .check_length(valid, 2, general = general)

    con <- valid[2] - 1 > valid[1]
    con <- ifelse(is.na(con), TRUE, con)

    if (con) {
      return(invisible(NULL))
    }

    general <- paste(
      pre,
      "If `interval` is `TRUE`,",
      "`valid[2]` must be larger than `valid[1] + 1`,",
      "if both are not `NA`."
    )

    .check_content(valid, "FALSE", NULL, general)
  }
}
