.check_type <- function(x, valid, name = NULL, general = NULL,
                        specifics = NULL, supplement = NULL, ...) {
  # check argument
  type <- typeof(x)

  if (type %in% valid) {
    return(invisible(NULL))
  }

  # prepare error message
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(general)) {
    general <- "`{name}` must have type { join(valid) }."
  }

  if (is.null(specifics)) {
    specifics = "`{name}` has type {type}."
  }

  # trigger error
  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


#' @template template
#'
#' @title Check Argument's Type
#'
#' @description Check if an argument has valid type,
#' and if not, generate an error message.
#'
#' @param valid A character vector which contains the valid types.
#'
#' @return An invisible `NULL` if the argument has valid type,
#' or an error message is generated.
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
#' # customize error message
#' check_type(
#'   arg,
#'   "character",
#'   general = "General statement of the error.",
#'   specifics = c(x = "An error.", i = "A hint.", `*` = "Other detail."),
#'   supplement = "More words to say."
#' )
#' }
#'
#' # add and retrieve additional arguments
#' tryCatch(
#'   {check_type(arg, "character", your_arg = "your data")},
#'   error = function(e) e$your_arg
#' )
check_type <- function(x, valid, name = NULL, general = NULL,
                       specifics = NULL, supplement = NULL, ...) {
  .check_type(valid, "character")
  check_arguments(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  .check_type(x, valid, name, general, specifics, supplement, ...)
}


#' @rdname validators
#' @order 3
#' @export
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
