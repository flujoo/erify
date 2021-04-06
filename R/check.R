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


is_integer <- function(x) {
  con <- is.numeric(x) && all(as.integer(x) == x)
  ifelse(is.na(con), TRUE, con)
}


check_length_valid <- function(valid, interval) {
  #
  general <- "`valid` must be a numeric vector of non-negative integers."

  .check_type(valid, c("integer", "double"), general = general)
  .check_length(valid, c(0, NA), general = general)

  specific <- "valid[{i}] is {v}."
  specifics <- character(0)

  for (i in 1:length(valid)) {
    v <- valid[i]
    con <- is_integer(v) && (is.na(v) || v >= 0)

    if (!con) {
      specifics %<>% c(glue(specific))
    }
  }

  if (length(specifics) > 0) {
    .Statement(general, specifics) %>% .trigger()
  }

  #
  if (isFALSE(interval)) {
    if (all(is.na(valid))) {
      general <-
        "If `interval` is `FALSE`, `valid` must not contain only `NA`."
      specifics <-
        "`valid` is {valid}."

      valid %<>% as_code(env = list(x = valid))

      .Statement(general, specifics, env = environment()) %>%
        .trigger()
    }
  }

  if (isTRUE(interval)) {
    general <- "If `interval` is `TRUE`, `valid` must have length 2."
    .check_length(valid, 2, general = general)

    con <- valid[2] - 1 > valid[1]
    con <- ifelse(is.na(con), TRUE, con)

    if (con) {
      return(invisible(NULL))
    }

    general <- paste(
      "If `interval` is `TRUE`,",
      "`valid[2]` must be larger than `valid[1] + 1,",
      "if both are not `NA`."
    )

    specifics <-
      "`valid` is {valid}."

    valid %<>% as_code(env = list(x = valid))

    .Statement(general, specifics, env = environment()) %>%
      .trigger()
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
#' - [check_class()] checks if an argument has valid class.
#' - [check_length()] checks if an argument has valid length.
#'
#' @param x The argument to be checked.
#'
#' @param valid
#' - In [check_type()]: a character vector which contains valid types.
#' - In [check_class()]: a character vector which contains valid classes.
#' - In [check_length()]: a numeric vector of non-negative integers
#' which represents the valid lengths. See argument `interval` below.
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
#' @param interval Only used in [check_length()].
#'
#' Optional. `TRUE` or `FALSE` which indicates if argument `valid` is
#' interpreted as an interval or as single lengths. For example, `c(1, 10)`
#' is interpreted as "larger than 1 and smaller than 10" if `interval`
#' is `TRUE`, but as "1 or 10" if `FALSE`.
#'
#' `NA` can be used in `valid`. For example, `c(0, NA)` means
#' "larger than 0".
#'
#' By default, `valid` is interpreted automatically.
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



# exported ----------------------------------------------------------------

#' @rdname check_argument
#' @export
check_type <- function(x, valid, name = NULL, general = NULL,
                       specifics = NULL, supplement = NULL, ...) {
  .check_type(valid, "character")
  check_error(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  .check_type(x, valid, name, general, specifics, supplement, ...)
}


#' @rdname check_argument
#' @export
check_class <- function(x, valid, name = NULL, general = NULL,
                        specifics = NULL, supplement = NULL, ...) {
  .check_type(valid, "character")
  check_error(name, general, specifics, supplement)

  if (inherits(x, valid)) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  valid %<>% join()

  if (is.null(general)) {
    general <- "{name} must have class {valid}."
  }

  classes <- class(x)
  s_class <- ifelse(length(classes) == 1, "class", "classes")
  classes %<>% join("and")

  if (is.null(specifics)) {
    specifics = "{name} has {s_class} {classes}."
  }

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


#' @rdname check_argument
#' @export
check_length <- function(x, valid, interval = NULL, name = NULL,
                         general = NULL, specifics = NULL,
                         supplement = NULL, ...) {
  if (!is.null(interval)) {
    .check_type(interval, "logical")
    .check_in(interval, c(TRUE, FALSE))
  }

  check_length_valid(valid, interval)
  check_error(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  .check_length(
    x, valid, interval, name, general, specifics, supplement, ...)
}
