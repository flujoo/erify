#' @title Create `Statement` Object
#'
#' @description Create a `Statement` object.
#'
#' `Statement` objects are used to create structured normal, warning or
#' error messages.
#'
#' @param general A single character which gives a general statement
#' of a message.
#'
#' @param specifics Optional. A character vector which gives a list of details
#' of a message. If `specifics` is a named vector, the names are used to
#' create bullets. if the name is `"x"` or `"i"`, the bullet will be colored
#' and bold. Any item with no name will be named `"x"`. Argument
#' `decorate` is used to turn on/off this process of adding and decorating
#' bullets. See "Examples" section.
#'
#' @param env Optional. An environment or named list which is used
#' to evaluate the R code in the above arguments.
#' See "Examples" section and [glue::glue()].
#'
#' @param decorate Optional. `TRUE` or `FALSE` which indicates if to decorate
#' the bullets of `specifics`. The default value is `TRUE`.
#'
#' @param ... Optional. Additional arguments which can be passed to
#' [rlang::abort()] or related functions.
#'
#' @return A list of class `Statement`.
#'
#' @seealso [trigger()] for generating normal, warning and error messages.
#'
#' `vignette("erify")` for a gentle introduction to this package.
#'
#' @export
#'
#' @examples
#' # quick example
#' general <- "I am the general statement of the message."
#' specifics <- c("Detail 1.", i = "Detail 2.", `*` = "Detail 3")
#' Statement(general, specifics)
#'
#' # do not decorate bullets
#' Statement(general, specifics, decorate = FALSE)
#'
#' # use R code in message
#' Statement("`x` is `{x}`.", env = list(x = 1))
Statement <- function(general, specifics = NULL, env = NULL, decorate = NULL,
                      ...) {
  g <- getOption("erify.general")

  .check_string(general, general = g)

  if (!is.null(specifics)) {
    .check_type(specifics, "character", general = g)
  }

  check_env(env)

  if (!is.null(decorate)) {
    check_bool(decorate, general = getOption("erify.general"))
  }

  if (is.null(decorate)) {
    decorate <- TRUE
  }

  .Statement(general, specifics, env, decorate, ...)
}


check_env <- function(env) {
  if (is.null(env)) {
    return(invisible())
  }

  .check_type(
    env, c("environment", "list"), general = getOption("erify.general"))

  if (is.environment(env) || length(env) == 0 ) {
    return(invisible())
  }

  ns <- names(env)

  general <- paste(
    getOption("erify.prepend"),
    "If `env` is list, each item of it must have a name."
  )

  valid <- "!is.null(x)"
  specific <- "`names(env)` is `NULL`."
  .check_content(ns, valid, NULL, general, specific)

  valid <- 'x_i != ""'
  specific <- "`env[[{i}]]` has no name."
  .check_contents(ns, valid, NULL, general, specific)
}


#' @title Trigger `Statement` Object
#'
#' @description Generate a normal, warning or error message with a
#' `Statement` object.
#'
#' @param statement The `Statement` object to trigger.
#'
#' @param as Optional. `"error"`, `"warning"` or `"message"` which indicates
#' how to trigger the `Statement` object. The default value is `"error"`.
#'
#' @return Generate a normal, warning or error message.
#'
#' @seealso [Statement()] for creating `Statement` objects.
#'
#' `vignette("erify")` for a gentle introduction to this package.
#'
#' @export
#'
#' @examples
#' s <- Statement("general", letters[1:3])
#'
#' # generate a normal message
#' trigger(s, "message")
#'
#' \dontrun{
#' # generate an error message
#' trigger(s, "error")
#' }
trigger <- function(statement, as = NULL) {
  g <- getOption("erify.general")

  check_class(statement, "Statement", general = g)

  if (!is.null(as)) {
    check_content(as, c("error", "warning", "message"), general = g)
  }

  if (is.null(as)) {
    as <- "error"
  }

  .trigger(statement, as)
}
