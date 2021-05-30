#' @inherit check_type
#'
#' @title Check Each Item's Type
#'
#' @description Check if each item of an argument has valid type,
#' and if not, generate an error message.
#'
#' @param x The argument to check, which must be a list.
#'
#' @param specific Optional. A single character which gives a detailed
#' description of the error. [glue::glue()] syntax can be used, see
#' "Examples" section. By default, this is generated automatically.
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
#' # argument to check
#' arg <- as.list(1:10)
#'
#' check_types(arg, "character")
#'
#' # customize error message with `glue::glue()` syntax
#' specific <- "`{name}[[{i}]]` is an {feature}, oh my god!"
#' check_types(arg, "character",  specific = specific)
#' }
check_types <- function(x, valid, name = NULL, general = NULL,
                        specific = NULL, supplement = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(specific)) {
    specific <- "`{name}[[{i}]]` has type {feature}."
  }

  specifics <- character(0)

  for (i in seq_along(x)) {
    feature <- typeof(x[[i]])
    pass <- feature %in% valid

    if (!pass) {
      specifics <- c(specifics, glue::glue(specific))
    }
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have type { join(valid) }."
  }

  throw(general, specifics, environment(), ...)
}
