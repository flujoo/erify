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
