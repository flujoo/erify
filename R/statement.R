#' @title Create `Statement` Object
#'
#' @description Create a `Statement` object.
#'
#' `Statement` objects are structured representations of
#' normal, warning or error messages.
#'
#' @param general A single character which represents the general statement
#' of a message.
#'
#' @param specifics Optional. A character vector which represents the details
#' of a message. If `specifics` is a named vector, the names are used to
#' create bullets. if the name is `"x"` or `"i"`, the bullet will be colored
#' and bold. Any item with no name will be named with `"x"`. Argument
#' `decorate` is used to turn on/off this process of adding and decorating
#' bullets. See "Examples" section.
#'
#' @param env Optional. An environment or named list which is used
#' to evaluate the R code in the above three arguments.
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
#' [glue::glue()] for inserting R code into characters.
#'
#' [rlang::abort()] for adding additional arguments.
#'
#' `vignette("erify")` for a gentle introduction to this package.
#'
#' [The tidyverse style guide](https://style.tidyverse.org/error-messages.html)
#' for more details about the style behind `Statement` objects.
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
  check_arguments(NULL, general, specifics)
  check_env(env)

  if (!is.null(decorate)) {
    check_bool(decorate)
  }

  if (is.null(decorate)) {
    decorate <- TRUE
  }

  .Statement(general, specifics, env, decorate, ...)
}


check_env <- function(env) {
  if (is.null(env)) {
    return(invisible(NULL))
  }

  check_type(env, c("environment", "list"))

  if (is.list(env)) {
    general <- "If `env` is list, each item of it must be named."
    ns <- names(env)

    if (length(env) == 0) {
      return(invisible(NULL))

    } else if (is.null(ns)) {
      .Statement(general, "`names(env)` is `NULL`.") %>% .trigger()

    } else {
      specifics <- character(0)
      specific <- "`env[[{i}]]` has no name."

      for (i in 1:length(ns)) {
        if (ns[i] == "") {
          specifics %<>% c(glue(specific))
        }
      }

      if (length(specifics) != 0) {
        .Statement(general, specifics) %>% .trigger()
      }
    }
  }
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
#' @return An invisible `NULL`. A normal, warning or error message is
#' generated.
#'
#' @seealso [Statement()]
#'
#' @examples
#' s <- Statement("general", letters[1:3])
#'
#' # generate normal message
#' trigger(s, "message")
#'
#' \dontrun{
#' # generate error message
#' trigger(s, "error")
#' }
#' @export
trigger <- function(statement, as = NULL) {
  check_class(statement, "Statement")

  if (!is.null(as)) {
    .check_content(as, c("error", "warning", "message"))
  }

  if (is.null(as)) {
    as <- "error"
  }

  .trigger(statement, as)
}
