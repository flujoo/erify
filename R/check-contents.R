#' @inherit check_content
#'
#' @title Check Each Item's Content
#'
#' @description Check if each item of an argument is from some given choices
#' or satisfies some requirement, and if not, generate an error message.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(1, 2, 3)
#'
#' check_contents(x, c(4, 5))
#'
#' general = "Each item of `x` must be `NA`."
#'
#' # `valid` can be a function or R code
#' check_contents(x, is.na, general = general)
#' check_contents(x, "is.na(x_i)", general = general)
#' }
check_contents <- function(x, valid, name = NULL, general = NULL,
                           specific = NULL, supplement = NULL,
                           as_double = TRUE, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(specific)) {
    if (is.atomic(x)) {
      specific <- "`{name}[{i}]` is {x_i}."
    } else if (is.list(x)) {
      specific <- "`{name}[[{i}]]` is {x_i}."
    }
  }

  specifics <- character(0)

  for (i in seq_along(x)) {
    x_i <- x[[i]]

    if (is.function(valid)) {
      pass <- valid(x_i)
    } else if (is.expression(valid)) {
      pass <- eval(valid)
    } else if (is.character(valid) && length(valid) == 1) {
      pass <- eval(parse(text = valid))
    } else {
      pass <- x_i %in% valid
    }

    if (!pass) {
      x_i <- back_quote(x_i, as_double = as_double)
      specifics <- c(specifics, glue::glue(specific))
    }
  }

  if (is.null(general)) {
    s_valid <- phrase_valid_content(valid, as_double)

    if (!is.null(s_valid)) {
      general <- "Each item of `{name}` must be {s_valid}."
    }
  }

  throw(general, specifics, environment(), ...)
}
