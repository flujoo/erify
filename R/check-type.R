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
#' bullets, see [throw()]. By default, this is left empty.
#'
#' @param ... Optional. Additional arguments which can be retrieved with
#' [tryCatch()].
#'
#' @return returns an invisible `NULL` if the argument is valid, or
#' generates an error message.
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
#'
#' # turn off `specific`
#' options(erify.n = 0)
#' check_type(arg, "character")
#' }
#'
#' # add and retrieve additional argument
#' tryCatch(
#'   {check_type(arg, "character", your_arg = "your data")},
#'   error = function(e) e$your_arg
#' )
check_type <- function(x, valid, name = NULL, general = NULL,
                       specific = NULL, supplement = NULL, ...) {
  feature <- typeof(x)
  pass <- feature %in% valid

  if (pass) {
    return(invisible())
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(general)) {
    general <- "`{name}` must have type { join(valid) }."
  }

  if (is.null(specific)) {
    specific <- "`{name}` has type {feature}."

  } else if (length(specific) == 0) {
    # turn off `specific` with empty vector
    specific <- NULL
  }

  specifics <- c(specific, supplement)
  throw(general, specifics, environment(), ...)
}
