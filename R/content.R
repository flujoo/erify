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
  check_content_valid(valid)
  check_statement(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  .check_type(x, typeof(valid), name, general, specifics, supplement, ...)
  .check_length(x, 1, NULL, name, general, specifics, supplement, ...)
  .check_content(
    x, valid, name, general, specifics, supplement, code = FALSE, ...)
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
