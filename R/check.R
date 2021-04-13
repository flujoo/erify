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
#'
#' @param x The argument to be checked.
#' - In [check_types()] and [check_classes()], `x` must be a list.
#' - In [check_content()], `x` must be a single atomic.
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



# type --------------------------------------------------------------------

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


#' @rdname validators
#' @export
check_type <- function(x, valid, name = NULL, general = NULL,
                       specifics = NULL, supplement = NULL, ...) {
  .check_type(valid, "character")
  check_statement(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  .check_type(x, valid, name, general, specifics, supplement, ...)
}


#' @rdname validators
#' @export
check_types <- function(x, valid, name = NULL, general = NULL,
                        supplement = NULL, ...) {
  .check_type(x, "list")
  .check_type(valid, "character")
  check_statement(name, general, specifics = NULL, supplement)

  l <- length(x)

  if (l == 0) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("{name}")
  }

  specifics <- character(0)
  specific <- "`{name}[[{i}]]` has type {type}."

  for (i in 1:l) {
    x_i <- x[[i]]
    type <- typeof(x_i)

    if (!(type %in% valid)) {
      specifics <- c(specifics, glue(specific))
    }
  }

  if (length(specifics) == 0) {
    return(invisible(NULL))
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have type {s_valid}."
  }

  s_valid <- join(valid)

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


#' @rdname validators
#' @export
check_class <- function(x, valid, name = NULL, general = NULL,
                        specifics = NULL, supplement = NULL, ...) {
  .check_type(valid, "character")
  check_statement(name, general, specifics, supplement)

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


#' @rdname validators
#' @export
check_classes <- function(x, valid, name = NULL, general = NULL,
                          supplement = NULL, ...) {
  .check_type(x, "list")
  .check_type(valid, "character")
  check_statement(name, general, specifics = NULL, supplement)

  l <- length(x)

  if (l == 0) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("{name}")
  }

  specifics <- character(0)
  specific <- "`{name}[[{i}]]` has class {c_}."

  for (i in 1:l) {
    x_i <- x[[i]]
    c_ <- class(x_i)

    if (!inherits(x_i, valid)) {
      specifics <- c(specifics, glue(specific))
    }
  }

  if (length(specifics) == 0) {
    return(invisible(NULL))
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have class {s_valid}."
  }

  s_valid <- join(valid)

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}



# length ------------------------------------------------------------------

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


#' @rdname validators
#' @export
check_length <- function(x, valid, interval = NULL, name = NULL,
                         general = NULL, specifics = NULL,
                         supplement = NULL, ...) {
  if (!is.null(interval)) {
    .check_type(interval, "logical")
    .check_content(interval, c(TRUE, FALSE))
  }

  check_length_valid(valid, interval)
  check_statement(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  .check_length(
    x, valid, interval, name, general, specifics, supplement, ...)
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



# content -----------------------------------------------------------------

# `code` indicates if single character `valid` is treated as code
.check_content <- function(x, valid, name = NULL, general = NULL,
                           specifics = NULL, supplement = NULL,
                           code = TRUE, ...) {
  # check if `valid` is treated as code
  is_code <- is.character(valid) &&
    length(valid) == 1 &&
    code

  # first two clauses are only for internal use
  if (is.function(valid)) {
    con <- valid(x)
  } else if (is_code) {
    con <- eval(parse(text = valid))
  } else {
    con <- x %in% valid
  }

  if (con) {
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

  if (!is_code && !is.function(valid)) {
    if (is.integer(valid)) {
      valid %<>% as.double()
    }

    valid %<>%
      as_code(TRUE) %>%
      join()
  }

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


#' @rdname validators
#' @export
check_content <- function(x, valid, name = NULL, general = NULL,
                          specifics = NULL, supplement = NULL, ...) {
  check_content_x(x)
  check_content_valid(valid)
  check_statement(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  .check_content(
    x, valid, name, general, specifics, supplement, code = FALSE, ...)
}


check_content_x <- function(x) {
  general <- "`x` must be a single atomic."

  if (!is.atomic(x)) {
    type <- typeof(x)

    .Statement(
      general,
      specifics = "`x` has type {type}.",
      env = environment()
    ) %>% .trigger()

  } else {
    l <- length(x)

    if (l != 1) {
      .Statement(
        general,
        specifics = "`x` has length {l}.",
        env = environment()
      ) %>% .trigger()
    }
  }
}


check_content_valid <- function(valid) {
  general <- "`valid` must be a non-empty atomic vector."

  if (!is.atomic(valid)) {
    type <- typeof(valid)

    .Statement(
      general,
      specifics = "`valid` has type {type}.",
      env = environment()
    ) %>% .trigger()

  } else {
    l <- length(valid)

    if (l == 0) {
      .Statement(
        general,
        specifics = "`valid` has length 0.",
        env = environment()
      ) %>% .trigger()
    }
  }
}


# single character --------------------------------------------------------

.check_string <- function(x, name = NULL, general = NULL, specifics = NULL,
                          supplement = NULL, ...) {
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


#' @rdname validators
#' @export
check_string <- function(x, name = NULL, general = NULL, specifics = NULL,
                         supplement = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  .check_string(x, name, general, specifics, supplement, ...)
}



# single positive integer -------------------------------------------------

#' @rdname validators
#' @export
check_n <- function(x, name = NULL, general = NULL, specifics = NULL,
                    supplement = NULL, ...) {
  check_statement(name, general, specifics, supplement)

  if (is_single_positive_integer(x)) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  if (is.null(general)) {
    general <- "{name} must be a single positive integer."
  }

  .check_type(
    x, c("double", "integer"), name, general, specifics, supplement, ...)

  .check_length(x, 1, NULL, name, general, specifics, supplement, ...)

  if (is.na(x)) {
    specifics <- "{name} is `NA`."
    .Statement(general, specifics, supplement, environment(), ...) %>%
      .trigger()
  }

  if (as.integer(x) != x || x <= 0) {
    specifics <- "{name} is `{x}`."
    .Statement(general, specifics, supplement, environment(), ...) %>%
      .trigger()
  }
}


is_single_positive_integer <- function(x) {
  is_integer(x) && length(x) == 1 && !is.na(x) && x > 0
}



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
