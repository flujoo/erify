#' @inherit check_type
#'
#' @title Check Each Item's Type
#'
#' @description Check if each item of an argument has valid type,
#' and if not, generate an error message.
#'
#' @param x The argument to check, which must be a list.
#'
#' @param specific Optional. A single character which gives the template for
#' reporting each invalid item. R expressions can be inserted with
#' [glue::glue()] syntax. See "Details" section for more details. By default,
#' this argument is generated automatically.
#'
#' @param n Optional. A positive integer which indicates how many invalid
#' items at most to display. The default value is `5`.
#'
#' @details The R expressions in argument `specific` can contain
#'
#' - argument `name`,
#' - argument `i` which indicates the index of an invalid item, and
#' - argument `type` which indicates the item's type.
#'
#' @seealso "Examples" section in [check_type()] for how to customize error
#' message and how to add additional arguments.
#'
#' [glue::glue()] for inserting R expressions into characters.
#'
#' @template seealso
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
#' # specify argument `specific`
#' check_types(
#'   arg,
#'   "character",
#'   specific = "`{name}[[{i}]]` is an {type}, oh my god!"
#' )
#'
#' # change argument `n`
#' check_types(arg, "character", n = 3)
#' }
check_types <- function(x, valid, name = NULL, general = NULL,
                        specific = NULL, supplement = NULL, n = NULL, ...) {
  # check arguments
  .check_type(x, "list")
  .check_type(valid, "character")
  check_arguments(name, general, NULL, supplement, specific, n)

  l <- length(x)

  # return silently if `x` is empty
  if (l == 0) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  specifics <- character(0)

  if (is.null(specific)) {
    specific <- "`{name}[[{i}]]` has type {type}."
  }

  for (i in 1:l) {
    type <- typeof(x[[i]])

    if (type %in% valid) {
      next
    }

    specifics %<>% c(glue::glue(specific))
  }

  if (length(specifics) == 0) {
    return(invisible(NULL))
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have type { join(valid) }."
  }

  if (is.null(n)) {
    n <- 5
  }

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger(n = n)
}
