#' @inherit check_type
#'
#' @title Check If Argument Is Single Character
#'
#' @description Check if an argument is a single character. and if not,
#' generate an error message.
#'
#' Can be used to check argument names, for example.
#'
#' @param specific Optional. A single character which gives a detailed
#' description of the error. By default, this is generated automatically.
#'
#' @seealso "Examples" section in [check_type()] for how to customize
#' error message and how to add and retrieve additional arguments.
#'
#' `vignette("erify")` for a gentle introduction to this package.
#'
#' @export
#'
#' @examples
#' x <- "a"
#' check_string(x)
#'
#' \dontrun{
#' # `x` must have type character
#' x <- c
#' check_string(x)
#'
#' # `x` must have length 1
#' x <- c("a", "b")
#' check_string(x)
#'
#' # `NA_character_` is not acceptable
#' x <- NA_character_
#' check_string(x)
#' }
check_string <- function(x, name = NULL, general = NULL, specific = NULL,
                         supplement = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(general)) {
    general <- "`{name}` must be a single character."
  }

  check_type(x, "character", name, general, specific, supplement, ...)
  check_length(x, 1, name, general, specific, supplement, ...)
  check_content(x, "!is.na(x)", name, general, specific, supplement, ...)
}
