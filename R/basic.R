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
