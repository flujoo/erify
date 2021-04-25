#' @title Check Argument's Type
#'
#' @description Check if an argument has valid type,
#' and if not, generate an error message.
#'
#' @param x The argument to check, which can be any object.
#'
#' @param valid A character vector which contains the valid types.
#'
#' @param name A single character which gives the argument's name.
#' The name is used in the error message. By default, the name of the
#' argument passed to argument `x` is captured automatically.
#'
#' @param general Optional. A single character which is used to give a
#' general statement of the error incurred. By default, this is generated
#' automatically.
#'
#' @param specific Optional. A single character which gives a detailed
#' description of the error. [glue::glue()] syntax can be used, see
#' "Examples" section. By default, this is generated automatically.
#'
#' @param supplement Optional. A (named) character vector which gives some
#' additional information about the error. The names are used to create
#' bullets, see [Statement()]. By default, this is left empty.
#'
#' @param feature Optional. A single character represents argument `x`'s
#' type. By default, this is inferred from `x`.
#'
#' @param ... Optional. Additional arguments passed to [rlang::abort()],
#' which is called internally.
#'
#' @return An invisible `NULL` if the argument is valid, or an error message
#' is generated.
#'
#' @seealso `vignette("erify")` for a gentle introduction to this package.
#'
#' @export
#'
#' @examples
#' # argument to check
#' arg <- 10
#'
#' # returns silently if the argument has valid type
#' check_type(arg, "double")
#'
#' \dontrun{
#' check_type(arg, "character")
#'
#' # specify argument's name
#' check_type(arg, "character", name = "x")
#'
#' # specify argument `specific` with `glue::glue()` syntax
#' specific <- "`{name}`'s type is {feature}, which is wrong."
#' check_type(arg, "character", specific = specific)
#'
#' # specify argument `supplement`
#' supplement <- c("You're wrong.", i = "Check your code.")
#' check_type(arg, "character", supplement = supplement)
#' }
#'
#' # add and retrieve additional argument
#' tryCatch(
#'   {check_type(arg, "character", your_arg = "your data")},
#'   error = function(e) e$your_arg
#' )
check_type <- function(x, valid, name = NULL, general = NULL,
                       specific = NULL, supplement = NULL, feature = NULL,
                       ...) {
  # check arguments
  .check_type(valid, "character", general = getOption("erify.general"))
  check_arguments(name, general, specific, supplement)

  if (!is.null(feature)) {
    .check_string(feature, general = g)
  }

  # capture name
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  .check_type(x, valid, name, general, specific, supplement, feature, ...)
}



# content -----------------------------------------------------------------

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
#' differentiate between types double and integer. The default value is
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
#' # differentiate between types double and integer
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
