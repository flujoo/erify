# Statement ---------------------------------------------------------------

.Statement <- function(general, specifics = NULL, supplement = NULL,
                       env = NULL, decorate = TRUE, ...) {
  specifics %<>% normalize_specifics(decorate)

  # create Statement object
  list(
    general = general,
    specifics = specifics,
    supplement = supplement,
    env = env,
    ...
  ) %>% `class<-`("Statement")
}


#' @export
print.Statement <- function(x, silent = FALSE, ...) {
  # convert each part
  ss <- x$general

  specifics <- x$specifics
  if (length(specifics) != 0) {
    ss %<>% c(paste(specifics, collapse = "\n"))
  }

  ss %<>% c(x$supplement)

  s <- paste(ss, collapse = "\n\n")

  # render `s`
  env <- x$env
  if (!is.null(env)) {
    s %<>% glue(env)
  }

  if (silent) {
    s
  } else {
    cat(s, "\n")
    invisible(s)
  }
}


normalize_specifics <- function(specifics, decorate) {
  l <- length(specifics)

  if (l == 0) {
    return()
  }

  # check if is in R Markdown
  in_rmd <- isTRUE(getOption('knitr.in.progress'))
  # ANSI escape sequence not work in R Markdown

  ns <- names(specifics)

  if (is.null(ns)) {
    ns <- rep("", l)
  }

  for (i in 1:l) {
    n <- ns[i]

    if (decorate) {
      if (n %in% c("", "x")) {
        n <- ifelse(in_rmd, "✖ ", "\033[0;31m✖\033[0m ")
      } else if (n == "i") {
        n <- ifelse(in_rmd, "ℹ ", "\033[0;36mℹ\033[0m ")
      } else {
        n %<>% paste0(" ")
      }
    }

    specifics[i] %<>% paste0(n, .)
  }

  specifics %>% unname()
}


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
#' @param supplement Optional. A single character which represents the
#' supplementary message appended at the end.
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
#' `vignette("er", package = "er")` for a gentle introduction to this package.
#'
#' [The tidyverse style guide](https://style.tidyverse.org/error-messages.html)
#' for more details about the style behind `Statement` objects.
#'
#' @examples
#' # quick example
#' general <- "I am the general statement of the message."
#' specifics <- c("Detail 1.", i = "Detail 2.", `*` = "Detail 3")
#' supplement <- "I am the additional information."
#' Statement(general, specifics, supplement)
#'
#' # do not decorate bullets
#' Statement(general, specifics, supplement, decorate = FALSE)
#'
#' # use R code in message
#' Statement("`x` is `{x}`.", env = list(x = 1))
#' @export
Statement <- function(general, specifics = NULL, supplement = NULL,
                      env = NULL, decorate = NULL, ...) {
  check_statement(NULL, general, specifics, supplement)
  check_env(env)

  if (!is.null(decorate)) {
    check_bool(decorate)
  }

  if (is.null(decorate)) {
    decorate <- TRUE
  }

  .Statement(general, specifics, supplement, env, decorate, ...)
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



# trigger -----------------------------------------------------------------

shorten <- function(statement, n = 5) {
  specifics <- statement$specifics
  l <- length(specifics)

  if (l <= n) {
    return(statement)
  }

  statement$specifics <- specifics[1:n]

  supplement <- ifelse(
    l == n + 1,
    "... and 1 more problem.",
    paste("... and", l - n, "more problems.")
  )

  if (is.null(statement$supplement)) {
    statement$supplement <- supplement
  } else {
    statement$supplement %<>% paste0(supplement, "\n\n", .)
  }

  statement
}


is_empty <- function(statement) {
  statement %>%
    {length(.$general) == 0 && length(.$specifics) == 0}
}


.trigger <- function(statement, as = "error", n = 5) {
  if (is_empty(statement)) {
    return(invisible(NULL))
  }

  if (as != "message") {
    statement %<>% shorten(n)
  }

  s <- print(statement, silent = TRUE)

  statement[c("general", "specifics", "supplement", "env")] <- NULL
  args <- c(list(message = s), statement)

  f <- switch(
    as,
    "error" = rlang::abort,
    "warning" = rlang::warn,
    "message" = rlang::inform
  )

  do.call(f, args)
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
#' @param n Optional. A positive integer which indicates how many items of
#' `specifics` of the `Statement` object at most to display. Used when `as`
#' is `"error"` or `"warning"`. The default value is `5`.
#'
#' @return An invisible `NULL`. A normal, warning or error message is
#' generated.
#'
#' @seealso [Statement()]
#'
#' @examples
#' s <- Statement("general", letters[1:6])
#'
#' # generate error message
#' trigger(s, "error")
#'
#' # generate normal message
#' trigger(s, "message")
#'
#' # change `n`
#' trigger(s, n = 2)
#' @export
trigger <- function(statement, as = NULL, n = NULL) {
  check_class(statement, "Statement")

  if (!is.null(as)) {
    .check_content(as, c("error", "warning", "message"))
  }

  if (!is.null(n)) {
    check_n(n)
  }

  if (is.null(as)) {
    as <- "error"
  }

  if (is.null(n)) {
    n <- 5
  }

  .trigger(statement, as, n)
}
