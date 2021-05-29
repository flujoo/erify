.check_length <- function(x, valid, interval = NULL, name = NULL,
                          general = NULL, specific = NULL, supplement = NULL,
                          ...) {
  # normalize `interval`
  interval %<>% normalize_interval(valid)

  # extract feature
  feature <- length(x)

  # validity
  pass <- is_valid_length(feature, valid, interval)

  # early return
  if (pass) {
    return(invisible(NULL))
  }

  # capture `name`
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  # general
  .general <- glue::glue(
    "`{name}` must have length { phrase_valid_length(valid, interval) }.")

  if (is.null(general)) {
    general <- .general
  }

  # specific
  if (is.null(specific)) {
    specific <- "`{name}` has length {feature}."
  }

  # add `supplement`
  specifics <- c(specific, supplement)

  .Statement(general, specifics, environment(), ...) %>% .trigger()
}


.check_content <- function(x, valid, name = NULL, general = NULL,
                           specific = NULL, supplement = NULL,
                           as_double = TRUE, as_code = TRUE, ...) {
  # if evaluate single character as code
  is_code <- is.character(valid) && length(valid) == 1 && as_code

  # validity
  # first two clauses are only for internal use
  if (is.function(valid)) {
    pass <- valid(x)
  } else if (is.expression(valid)) {
    pass <- eval(valid)
  } else if (is_code) {
    pass <- eval(parse(text = valid))
  } else {
    pass <- x %in% valid
  }

  # early return
  if (pass) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  # general
  .general <- glue::glue(
    "`{name}` must be { phrase_valid_content(valid, as_double) }.")

  if (is.null(general)) {
    general <- .general
  }

  if (is.null(specific)) {
    specific <- "`{name}` is { .back_quote(x, as_double) }."
  }

  # add `supplement`
  specifics <- c(specific, supplement)

  .Statement(general, specifics, environment(), ...) %>% .trigger()
}


.check_string <- function(x, name = NULL, general = NULL, specific = NULL,
                          supplement = NULL, ...) {
  # capture `name`
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  # general
  .general <- glue::glue("`{name}` must be a single character.")

  if (is.null(general)) {
    general <- .general
  }

  .check_type(x, "character", name, general, specific, supplement, ...)
  .check_length(x, 1, NULL, name, general, specific, supplement, ...)
  .check_content(x, "!is.na(x)", name, general, specific, supplement, ...)
}


.check_contents <- function(x, valid, name = NULL, general = NULL,
                            specific = NULL, supplement = NULL,
                            as_double = TRUE, as_code = TRUE, n = NULL,
                            ...) {
  # if evaluate single character as code
  is_code <- is.character(valid) && length(valid) == 1 && as_code

  # capture name
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  # specific
  if (is.null(specific)) {
    if (is.atomic(x)) {
      specific <- "`{name}[{i}]` is {x_i}."
    } else if (is.list(x)) {
      specific <- "`{name}[[{i}]]` is {x_i}."
    }
  }

  specifics <- character(0)

  for (i in seq_along(x)) {
    x_i <- x[[i]]

    if (is.function(valid)) {
      pass <- valid(x_i)
    } else if (is.expression(valid)) {
      pass <- eval(valid)
    } else if (is_code) {
      pass <- eval(parse(text = valid))
    } else {
      pass <- x_i %in% valid
    }

    if (!pass) {
      x_i %<>% .back_quote(as_double)
      specifics %<>% c(glue::glue(specific))
    }
  }

  # early return
  if (length(specifics) == 0) {
    return(invisible())
  }

  # general
  .general <- glue::glue(
    "Each item of `{name}` must be ",
    "{ phrase_valid_content(valid, as_double) }."
  )

  if (is.null(general)) {
    general <- .general
  }

  if (is.null(n)) {
    n <- 5
  }

  specifics %<>%
    .shorten(n) %>%
    c(supplement)

  .Statement(general, specifics, environment(), ...) %>% .trigger()
}
