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
#' @param n Optional. A non-negative integer which indicates how many invalid
#' items at most to display. The default value is `5`.
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
#'
#' # change the number of invalid items to display
#' check_types(arg, "character", n = 0)
#' }
check_types <- function(x, valid, name = NULL, general = NULL,
                        specific = NULL, supplement = NULL, n = NULL, ...) {
  # check arguments
  .check_type(x, "list", general = getOption("erify.general"))
  check_type_valid(valid)
  check_arguments(name, general, specific, supplement, n)

  l <- length(x)

  # early return
  if (l == 0) {
    return(invisible())
  }

  # capture name
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  # specific
  if (is.null(specific)) {
    specific <- "`{name}[[{i}]]` has type {feature}."
  }

  specifics <- character(0)

  for (i in 1:l) {
    feature <- typeof(x[[i]])
    pass <- feature %in% valid

    if (!pass) {
      specifics %<>% c(glue::glue(specific))
    }
  }

  # early return
  if (length(specifics) == 0) {
    return(invisible())
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have type { .join(valid) }."
  }

  if (is.null(n)) {
    n <- 5
  }

  specifics %<>%
    .shorten(n) %>%
    c(supplement)

  .Statement(general, specifics, environment(), ...) %>% .trigger()
}


