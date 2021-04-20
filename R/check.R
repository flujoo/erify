# dummy -------------------------------------------------------------------

#' @name validators
#'
#' @title Functions for Checking Arguments
#'
#' @description When creating functions for other people to use, you always
#' need some validator functions which check if arguments passed by users are
#' valid, and if not, generate informative and good-formatted error messages
#' in a consistent style.
#'
#' Functions documented here serve the exact purposes:
#'
#' - [check_type()] checks if an argument has valid type.
#' - [check_types()] checks if each item of an argument has valid type.
#' - [check_class()] checks if an argument has valid class.
#' - [check_classes()] checks if each item of an argument has valid class.
#' - [check_length()] checks if an argument has valid length.
#' - [check_content()] checks if an argument is from some given choices.
#' - [check_string()] checks if an argument is a single character.
#' It can be used to check names, for example.
#' - [check_n()] checks if an argument is a single positive integer.
#' It can be used to check indices, for example.
#' - [check_bool()] checks if an argument is `TRUE` or `FALSE`.
#'
#' @param x The argument to be checked.
#' - In [check_types()] and [check_classes()], `x` must be a list.
#' - In other functions, `x` can be any object.
#'
#' @param valid
#' - In [check_type()] and [check_types()]: a character vector which
#' contains the valid types.
#' - In [check_class()] and [check_classes()]: a character vector which
#' contains the valid classes.
#' - In [check_length()]: a numeric vector which contains non-negative
#' integers or `NA`, used with argument `interval` to indicate the valid
#' lengths. See argument `interval` for more details.
#' - In [check_content()]: an atomic vector which contains the valid choices.
#'
#' @param name Optional. A single character which represents the argument's
#' name. The name is used in the error message. By default, the name of the
#' argument passed to argument `x` is captured automatically.
#'
#' @param general Optional. A single character which represents the general
#' statement of the error. Each validator function comes with its own
#' error message, so usually you don't need to specify this argument and
#' arguments `specifics` and `supplement`.
#'
#' @param specifics Optional. A (named) character vector which represents
#' details of the error.
#'
#' @param supplement Optional. A single character which represents the
#' additional message added at the end of the error message.
#'
#' @param ... Optional. Additional arguments passed to [rlang::abort()],
#' which is called internally.
#'
#' @param interval Only used in [check_length()]. Optional. `TRUE` or
#' `FALSE` which indicates if argument `valid` is interpreted as an
#' interval or as single lengths. For example, `c(1, 10)` is interpreted
#' as "larger than 1 and smaller than 10" if `interval` is `TRUE`,
#' but as "1 or 10" if `FALSE`. `NA` can be used in `valid` when treated
#' as interval. For example, `c(0, NA)` means "larger than 0". By default,
#' `interval` is inferred automatically.
#'
#' @return An invisible `NULL` if the argument is valid,
#' or error message is generated.
#'
#' @order 1
#'
#' @seealso [Statement()] for more details about arguments `general`,
#' `specifics` and `supplement`.
#'
#' [rlang::abort()] for adding additional arguments.
#'
#' `vignette("erify", package = "erify")` for a gentle introduction to this
#' package.
#'
#' [The tidyverse style guide](https://style.tidyverse.org/error-messages.html)
#' for more details about the used error message style.
#'
#' @examples
#' # argument to be checked
#' arg <- 1:10
#'
#' # invisible `NULL` returned if `arg` is valid
#' check_type(arg, "integer")
#'
#' \dontrun{
#' # check if `arg` is character or double
#' check_type(arg, c("character", "double"))
#'
#' # check if `arg` has length 1
#' check_length(arg, 1)
#'
#' # check if `arg` has length smaller than 3
#' check_length(arg, c(NA, 3))
#'
#' # specify argument's name
#' check_type(arg, "character", "`other_name`")
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
NULL



# utils -------------------------------------------------------------------

# this is black magic ...
as_code <- function(x, recursive = FALSE, env = environment()) {
  if (!recursive) {
    s <- deparse(substitute(x, env = env))
    # can't use %>% here

  } else {
    s <- character(0)

    for (x_i in x) {
      s_i <- deparse(substitute(x_i))
      s %<>% c(s_i)
    }
  }

  glue("`{s}`")
}


check_statement <- function(name, general, specifics, supplement) {
  if (!is.null(name)) {
    .check_string(name)
  }

  if (!is.null(general)) {
    .check_string(general)
  }

  if (!is.null(specifics)) {
    .check_type(specifics, "character")
  }

  if (!is.null(supplement)) {
    .check_string(supplement)
  }
}
