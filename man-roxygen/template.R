#' @param x The argument to check, which can be any object.
#'
#' @param name A single character which gives the argument's name.
#' The name is used in the error message. By default, the name of the
#' argument passed to argument `x` is captured automatically.
#'
#' @param general Optional. A single character which is used to give a
#' general statement of the error incurred. This is generated automatically
#' by default.
#'
#' @param specifics Optional. A (named) character vector which gives a
#' list of detailed descriptions of the error. This is generated automatically
#' by default.
#'
#' @param supplement Optional. A single character which gives some
#' additional information about the error. This is left empty by default.
#'
#' @param ... Optional. Additional arguments passed to [rlang::abort()],
#' which is called internally.
#'
#' @seealso [Statement()] for more details about arguments `general`,
#' `specifics` and `supplement`.
#'
#' [rlang::abort()] for adding additional arguments.
#'
#' `vignette("erify")` for a gentle introduction to this package.
#'
#' [The tidyverse style guide](https://style.tidyverse.org/error-messages.html)
#' for more details about the used error message style.
