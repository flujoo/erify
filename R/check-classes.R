#' @inherit check_types
#'
#' @title Check Each Item's Class
#'
#' @description Check if each item of an argument has valid class,
#' and if not, generate an error message.
#'
#' @param valid A character vector which contains valid classes.
#'
#' @export
#'
#' @examples
#' # argument to check
#' arg <- lapply(1:10, function(x) {class(x) <- c("a", "b"); x})
#'
#' check_classes(arg, "a")
#'
#' \dontrun{
#' check_classes(arg, c("x", "y"))
#' }
check_classes <- function(x, valid, name = NULL, general = NULL,
                          specific = NULL, supplement = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(specific)) {
    specific <- "`{name}[[{i}]]` has class { join(feature, 'and') }."
  }

  specifics <- character(0)

  for (i in seq_along(x)) {
    x_i <- x[[i]]
    pass <- inherits(x_i, valid)

    if (!pass) {
      feature <- class(x_i)
      specifics <- c(specifics, glue::glue(specific))
    }
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have class { join(valid) }."
  }

  throw(general, specifics, environment(), ...)
}


#' @inherit check_length
#'
#' @title Check Binary Operator's Arguments' Classes
#'
#' @description Check if the arguments of a binary operator have valid
#' classes, and if not, generate an error message.
#'
#' @param x,y The argument to check, which can be any object.
#'
#' @param valid_x,valid_y A character vector which contains the valid classes.
#' `valid_y` is assigned `valid_x`, if not specified.
#'
#' @param operator Optional. A single character which represents the binary
#' operator.
#'
#' @param commutative `TRUE` or `FALSE` which indicates if arguments `x` and
#' `y` can be swapped around. The default value is `TRUE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 1
#' class(x) <- c("a", "b")
#'
#' y <- 2
#' class(y) <- c("c", "d")
#'
#' check_binary_classes(x, y, c("d", "e"))
#' check_binary_classes(x, y, c("d", "e"), operator = "+")
#'
#' check_binary_classes(x, y, c("d", "e"), c("a", "f"))
#' check_binary_classes(x, y, c("d", "e"), c("a", "f"), commutative = FALSE)
#'
#' # customize error message with `glue::glue()` syntax
#' check_binary_classes(
#'   x, y, c("d", "e"),
#'   specific = "Left: {feature_x[1]}, {feature_x[2]}.",
#'   supplement = "Right: {feature_y[1]}, {feature_y[2]}."
#' )
#' }
check_binary_classes <- function(x, y, valid_x, valid_y = NULL,
                                 operator = NULL, commutative = NULL,
                                 general = NULL, specific = NULL,
                                 supplement = NULL, ...) {
  if (is.null(commutative)) {
    commutative <- TRUE
  }

  # general
  if (is.null(general)) {
    # phrase `operator`
    if (is.null(operator)) {
      s_operator <- ""
    } else {
      s_operator <- glue::glue(" of `{operator}`")
    }

    # based on `valid_y`
    if (is.null(valid_y)) {
      general <- "Both sides{s_operator} must have class { join(valid_x) }."
    } else {
      # based on `commutative`
      if (commutative) {
        general <- paste(
          "One side{s_operator} must have class { join(valid_x) },",
          "the other side { join(valid_y) }."
        )
      } else {
        general <- paste(
          "The left side{s_operator} must have class { join(valid_x) },",
          "the right side { join(valid_y) }."
        )
      }
    }
  }

  # normalize arguments
  if (is.null(valid_y)) {
    valid_y <- valid_x
  }

  # validity
  pass <- inherits(x, valid_x) && inherits(y, valid_y)

  if (commutative) {
    pass <- any(pass, inherits(y, valid_x) && inherits(x, valid_y))
  }

  if (pass) {
    return(invisible())
  }

  # extract feature
  feature_x <- class(x)
  feature_y <- class(y)

  # specifics
  if (is.null(specific)) {
    specific <- paste(
      "The left side has class { join(feature_x, 'and') },",
      "right side { join(feature_y, 'and') }."
    )
  }

  specifics <- c(specific, supplement)
  throw(general, specifics, environment(), ...)
}
