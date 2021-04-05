# internal ----------------------------------------------------------------

.check_type <- function(x, valid, name = NULL, general = NULL,
                        specifics = NULL, supplement = NULL, ...) {
  type <- typeof(x)

  if (type %in% valid) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  valid %<>% join()

  if (is.null(general)) {
    general <- "{name} must have type {valid}."
  }

  if (is.null(specifics)) {
    specifics = "{name} has type {type}."
  }

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


.check_length <- function(x, valid, interval = NULL, name = NULL,
                          general = NULL, specifics = NULL,
                          supplement = NULL, ...) {
  l <- length(x)
  interval %<>% normalize_interval(valid)

  if (is_valid_length(l, valid, interval)) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  valid %<>% phrase_valid_length(interval)

  if (is.null(general)) {
    general <- "{name} must have length {valid}."
  }

  if (is.null(specifics)) {
    specifics = "{name} has length {l}."
  }

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


.check_in <- function(x, valid, name = NULL, general = NULL,
                      specifics = NULL, supplement = NULL, ...) {
  if (x %in% valid) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  # treat integers as doubles
  if (is.integer(x)) {
    x %<>% as.double()
  }

  if (is.integer(valid)) {
    valid %<>% as.double()
  }

  valid %<>%
    as_code(TRUE) %>%
    join()

  x %<>% as_code(env = list(x = x))

  if (is.null(general)) {
    general <- "{name} must be {valid}."
  }

  if (is.null(specifics)) {
    specifics = "{name} is {x}."
  }

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


.check_single_character <- function(x, name = NULL, general = NULL,
                                    specifics = NULL, supplement = NULL,
                                    ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  if (is.null(general)) {
    general <- "{name} must be a single character."
  }

  .check_type(x, "character", name, general, specifics, supplement, ...)
  .check_length(x, 1, NULL, name, general, specifics, supplement, ...)
}



# utils -------------------------------------------------------------------

normalize_interval <- function(interval, valid) {
  if (!is.null(interval)) {
    return(interval)
  }

  l <- length(valid)

  if (l != 2) {
    FALSE

  } else {
    con <- valid[2] > (valid[1] + 1)
    ifelse(is.na(con), TRUE, con)
  }
}


is_valid_length <- function(l, valid, interval) {
  if (!interval) {
    l %in% valid

  } else {
    con <- l > valid[1] && l < valid[2]
    ifelse(is.na(con), TRUE, con)
  }
}


phrase_valid_length <- function(valid, interval) {
  if (!interval) {
    join(valid)

  } else {
    v1 <- valid[1]
    v2 <- valid[2]
    ss <- character(0)

    if (!is.na(v1)) {
      ss %<>% c(glue("larger than {v1}"))
    }

    if (!is.na(v2)) {
      ss %<>% c(glue("smaller than {v2}"))
    }

    join(ss, "and")
  }
}


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


check_error <- function(name, general, specifics, supplement) {
  if (!is.null(name)) {
    .check_single_character(name)
  }

  if (!is.null(general)) {
    .check_single_character(general)
  }

  if (!is.null(specifics)) {
    .check_type(specifics, "character")
  }

  if (!is.null(supplement)) {
    .check_single_character(supplement)
  }
}



# dummy -------------------------------------------------------------------

#' @name check_argument
#'
#' @title Check Argument
#'
#' @description Check if a passed argument is valid.
#'
#' When creating functions for other people to use, you always need
#' some validator functions which
#'
#' - check if arguments passed by users are valid, and if not,
#' - generate informative and good-formatted error messages in a
#' consistent style.
#'
#' These following functions serve the exact purpose:
#'
#' - [check_type()] checks if an argument has valid type.
#'
#' @param x The argument to be checked.
#'
#' @param valid
#' - In [check_type()]: a character vector which contains valid types.
#'
#' @param name A single character which represents the argument's name.
#' Used in error message. If not specified, the name is captured
#' automatically.
#'
#' @param general Optional. A single character which represents the general
#' statement of the error. See [Statement()] for more details about
#' `general`, `specifics` and `supplement`.
#'
#' @param specifics Optional. A (named) character vector which contains
#' details of the error.
#'
#' @param supplement Optional. A single character which represents the
#' additional message added at the end of the error message.
#'
#' @param ... Optional. Additional arguments passed to [rlang::abort()],
#' which is called internally.
#'
#' @return An invisible `NULL` if the argument has valid type,
#' or error message is generated.
#'
#' @seealso [Statement()] for more details about arguments `general`,
#' `specifics` and `supplement`.
#'
#' [rlang::abort()] for adding additional arguments.
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
#' )}
#'
#' # add and retrieve additional arguments
#' tryCatch(
#'   {check_type(arg, "character", your_arg = "your data")},
#'   error = function(e) e$your_arg
#' )
NULL
