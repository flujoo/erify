# single character --------------------------------------------------------

#' @rdname validators
#' @order 8
#' @export
check_string <- function(x, name = NULL, general = NULL, specifics = NULL,
                         supplement = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  .check_string(x, name, general, specifics, supplement, ...)
}



# single positive integer -------------------------------------------------

#' @rdname validators
#' @order 9
#' @export
check_index <- function(x, name = NULL, general = NULL, specifics = NULL,
                    supplement = NULL, ...) {
  check_statement(name, general, specifics, supplement)

  if (is_single_positive_integer(x)) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(general)) {
    general <- "`{name}` must be a single positive integer."
  }

  .check_type(
    x, c("double", "integer"), name, general, specifics, supplement, ...)

  .check_length(x, 1, NULL, name, general, specifics, supplement, ...)

  if (is.na(x)) {
    specifics <- "`{name}` is `NA`."
    .Statement(general, specifics, supplement, environment(), ...) %>%
      .trigger()
  }

  if (as.integer(x) != x || x <= 0) {
    specifics <- "`{name}` is `{x}`."
    .Statement(general, specifics, supplement, environment(), ...) %>%
      .trigger()
  }
}


is_single_positive_integer <- function(x) {
  is_integer(x) && length(x) == 1 && !is.na(x) && x > 0
}



# single bool -------------------------------------------------------------

#' @rdname validators
#' @order 10
#' @export
check_bool <- function(x, name = NULL, general = NULL, specific = NULL,
                       supplement = NULL, ...) {
  check_arguments(name, general, specific, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  check_content(x, c(TRUE, FALSE), name, general, specific, supplement, ...)
}
