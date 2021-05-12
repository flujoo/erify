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


#' @title Connect Words with Conjunction
#'
#' @description Connect given words with a conjunction, e.g. "and" and
#' "or".
#'
#' @param words A vector of list whose items can be converted to characters.
#'
#' @param conjunction A single character which represents a conjunction word.
#' The default value is `"or"`.
#'
#' @return If has length 1 or less, `words` is returned. Or items of `words`
#' are concatenated and returned.
#'
#' @export
#'
#' @examples
#' words <- c("apple", "orange", "Pink Floyd")
#' join(words, "and")
join <- function(words, conjunction = NULL) {
  if (!is.null(conjunction)) {
    check_string(conjunction)
  }

  if (is.null(conjunction)) {
    conjunction <- "or"
  }

  .join(words, conjunction)
}


#' @title Back Quote Object
#'
#' @description Convert an R object to character and add back quotations.
#'
#' @inheritParams check_content
#'
#' @param x An R object.
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples
#' back_quote(1:3)
#'
#' back_quote(1:3, FALSE)
#'
#' back_quote(NULL)
#'
#' back_quote(list(c, 1:3, "a"))
back_quote <- function(x, as_double = NULL) {
  if (!is.null(as_double)) {
    check_bool(as_double)
  }

  if (is.null(as_double)) {
    as_double <- TRUE
  }

  .back_quote(x, as_double)
}


#' @title Shorten Message
#'
#' @description Shorten a list of message details.
#'
#' @param specifics A character vector which gives a list of message
#' details.
#'
#' @param n Optional. A positive integer which indicates how many message
#' details at most to display. The default value is `5`.
#'
#' @return A character vector.
#'
#' @seealso Usually used with [Statement()] and [trigger()].
#'
#' @export
#'
#' @examples
#' ss <- letters[1:6]
#'
#' shorten(ss)
#'
#' shorten(ss, 3)
#'
#' shorten(ss, 7)
shorten <- function(specifics, n = NULL) {
  .check_type(specifics, "character")

  if (!is.null(n)) {
    check_n(n)
  }

  if (is.null(n)) {
    n <- 5
  }

  .shorten(specifics, n)
}


#' @title Shortcut to Trigger Message
#'
#' @description A shortcut to trigger a message.
#'
#' @inheritParams Statement
#'
#' @param specifics Optional. A character vector which gives a list of details
#' of a message. If is `character(0)`, `throw()` returns silently.
#'
#' @param env Optional. An environment or named list which is used to evaluate
#' the R code in the above arguments.
#'
#' @param n Optional. A positive integer which indicates how many message
#' details at most to display. The default value is `5`.
#'
#' @param as Optional. `"error"`, `"warning"` or `"message"` which indicates
#' how to trigger the message. The default value is `"error"`.
#'
#' @return If `specifics` is `character(0)`, returns an invisible `NULL`.
#' Or generate a normal, warning, or error message.
#'
#' @seealso This function is a shortcut of [shorten()], [Statement()] and
#' [trigger()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' general <- "You are wrong."
#' throw(general)
#'
#' # returns silently if `specifics` is `character(0)`
#' throw(general, character(0))
#'
#' # display less details
#' throw(general, letters, n = 3)
#' }
throw <- function(general, specifics = NULL, env = NULL, n = NULL, as = NULL,
                  ...) {
  # check arguments
  g <- getOption("erify.general")

  .check_string(general, general = g)

  if (!is.null(specifics)) {
    .check_type(specifics, "character", general = g)
  }

  check_env(env)

  if (!is.null(n)) {
    pre <- getOption("erify.prepend")
    g_n <- paste(pre, "`{name}` must be a positive integer.")
    check_n(n, general = g_n)
  }

  if (!is.null(as)) {
    check_content(as, c("error", "warning", "message"), general = g)
  }

  # normalize arguments
  if (is.null(n)) {
    n <- 5
  }

  if (is.null(as)) {
    as <- "error"
  }

  # return silently if specifics is `character(0)`
  if (identical(specifics, character(0))) {
    return(invisible())
  }

  # main
  specifics %>%
    .shorten(n) %>%
    .Statement(general, ., env, ...) %>%
    .trigger(as)
}
