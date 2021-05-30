#' @title Generate and Signal Condition
#'
#' @description Generate and signal a condition.
#'
#' @param general A single character which gives a general statement of
#' the condition.
#'
#' @param specifics Optional. A character vector which gives a list of details
#' of the condition. If is `character(0)`, `throw()` will return silently.
#' If is a named vector, the names are used to create bullets. If the name is
#' `"x"` or `"i"`, the bullet will be colored and bold. The default name is
#' `"x"`. You can customize bullets with option `erify.bullets`.
#'
#' @param env Optional. An environment or named list which is used to evaluate
#' the R code in the above arguments. See [glue::glue()].
#'
#' @param as Optional. `"error"`, `"warning"` or `"message"` which indicates
#' how to signal the condition. The default value is `"error"`.
#'
#' @param class Optional. A character vector which assigns classes to the
#' condition.
#'
#' @param ... Optional. Additional arguments which are stored in the
#' condition and can be retrieved with [tryCatch()].
#'
#' @return If `specifics` is `character(0)`, returns an invisible `NULL`.
#' Or signals an error, a warning, or a message.
#'
#' @export
#'
#' @examples
#' general <- "You are wrong."
#'
#' # returns silently
#' throw(general, character(0))
#'
#' \dontrun{
#' throw(general)
#'
#' specifics <- c("Detail 1.", i = "Detail 2.")
#' throw(general, specifics)
#'
#' # embed R code with glue syntax
#' throw("`x` is {x}.", env = list(x = 1))
#' }
#'
#' # add and retrieve additional argument
#' tryCatch(
#'   { throw(general, arg = "I'm an additional argument.") },
#'   error = function(e) e$arg
#' )
throw <- function(general, specifics = NULL, env = NULL, as = "error",
                  class = NULL, ...) {
  # return silently if `specifics` is empty
  # this is useful in checking items of vectors
  pass <- !is.null(specifics) && length(specifics) == 0

  if (pass) {
    return(invisible())
  }

  to_string(general, specifics, env, getOption("erify.n")) |>
    generate_condition(as, class, ...) |>
    trigger(as)
}


# prepend bullet to each item of `specifics`
decorate_specifics <- function(specifics) {
  bullets <- getOption("erify.bullets")

  for (i in seq_along(specifics)) {
    specific <- specifics[i]
    name <- names(specific)

    # prepend bullet
    if (is.null(name) || name %in% c("", "x")) {
      bullet <- bullets$x
    } else if (name == "i") {
      bullet <- bullets$i
    } else {
      bullet <- name
    }

    specifics[i] <- paste(bullet, specific)
  }

  specifics
}


# shorten `specifics` if its length is larger than `n`
shorten <- function(specifics, n = 5L) {
  if (n == 0) {
    return()
  }

  l <- length(specifics)

  if (l <= n) {
    return(specifics)
  }

  specifics <- specifics[1:n]

  supplement <- ifelse(
    l == n + 1,
    "... and 1 more problem.",
    paste("... and", l - n, "more problems.")
  )

  c(specifics, supplement)
}


# combine `general` and `specifics` to a printable string
to_string <- function(general, specifics = NULL, env = NULL, n = 5L) {
  if (!is.null(specifics)) {
    specifics <- specifics |>
      shorten(n) |>
      decorate_specifics() |>
      paste(collapse = "\n")

    # combine `general` and `specifics`
    general <- paste(general, specifics, sep = "\n\n")
  }

  if (!is.null(env)) {
    general <- glue::glue(general, .envir = env)
  }

  unclass(general)
}


# check `?conditions`
generate_condition <- function(message, as = "error", class = NULL, ...) {
  classes <- c(class, as, "condition")

  list(
    message = message,
    ...
  ) |> `class<-`(classes)
}


# trigger `condition`
trigger <- function(condition, as = "error") {
  if (as == "error") {
    stop(condition)

  } else if (as == "warning") {
    warning(condition)

  } else if (as == "message") {
    message(condition)
  }
}
