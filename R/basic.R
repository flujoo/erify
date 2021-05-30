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
