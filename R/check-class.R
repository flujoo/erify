# class -------------------------------------------------------------------

#' @inherit check_length
#'
#' @title Check Argument's Class
#'
#' @description Check if an argument has valid class, and if not,
#' generate an error message.
#'
#' @param valid A character vector which contains valid classes.
#'
#' @export
#'
#' @examples
#' x <- 1
#' class(x) <- c("a", "b")
#'
#' check_class(x, c("a", "c"))
#'
#' \dontrun{
#' check_class(x, c("c", "d"))
#'
#' # customize error message with `glue::glue()` syntax
#' specific <- "Unbelievable! The first class of `{name}` is {feature[1]}."
#' check_class(x, c("c", "d"), specific = specific)
#' }
check_class <- function(x, valid, name = NULL, general = NULL,
                        specific = NULL, supplement = NULL, ...) {
  pass <- inherits(x, valid)

  if (pass) {
    return(invisible())
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  feature <- class(x)

  if (is.null(general)) {
    general <- "`{name}` must have class { join(valid) }."
  }

  if (is.null(specific)) {
    specific <- "`{name}` has class { join(feature, 'and') }."

  } else if (length(specific) == 0) {
    # turn off `specific` with empty vector
    specific <- NULL
  }

  specifics <- c(specific, supplement)
  throw(general, specifics, environment(), ...)
}
