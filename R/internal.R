# utils -------------------------------------------------------------------

.print_string <- function(string, silent = FALSE) {
  if (silent) {
    string

  } else {
    cat(string, "\n")
    invisible(string)
  }
}


.join <- function(words, conjunction = "or") {
  l <- length(words)

  if (l <= 1) {
    return(words)
  }

  paste(
    paste(words[-l], collapse = ", "),
    conjunction,
    words[l]
  )
}


.back_quote <- function(x, as_double = TRUE) {
  recursive <- (is.atomic(x) || is.list(x)) &&
    length(x) > 0

  if (!recursive) {
    ss <- deparse(x)

  } else {
    if (as_double && is.integer(x)) {
      x %<>% as.double()
    }

    ss <- sapply(x, deparse, USE.NAMES = FALSE)
  }

  glue::glue("`{ss}`") %>% unclass()
}


# shortcut to check commonly used arguments
check_arguments <- function(name = NULL, general = NULL, specific = NULL,
                            supplement = NULL, n = NULL) {
  g <- getOption("erify.general")

  if (!is.null(name)) {
    .check_string(name, general = g)
  }

  if (!is.null(general)) {
    .check_string(general, general = g)
  }

  if (!is.null(specific)) {
    .check_type(specific, "character", general = g)
    .check_length(specific, c(0, 1), general = g)
    # so that you can turn off `specific` with `character(0)`
  }

  if (!is.null(supplement)) {
    .check_type(supplement, "character", general = g)
  }

  if (!is.null(n)) {
    pre <- getOption("erify.prepend")
    g <- paste(pre, "`{name}` must be a single non-negative integer.")
    check_n(n, general = g, zero = TRUE)
  }
}


check_env <- function(env) {
  if (is.null(env)) {
    return(invisible())
  }

  .check_type(
    env, c("environment", "list"), general = getOption("erify.general"))

  if (is.environment(env) || length(env) == 0 ) {
    return(invisible())
  }

  ns <- names(env)

  general <- paste(
    getOption("erify.prepend"),
    "If `env` is list, each item of it must have a name."
  )

  valid <- "!is.null(x)"
  specific <- "`names(env)` is `NULL`."
  .check_content(ns, valid, NULL, general, specific)

  valid <- 'x_i != ""'
  specific <- "`env[[{i}]]` has no name."
  .check_contents(ns, valid, NULL, general, specific)
}



# Statement ---------------------------------------------------------------

normalize_specifics <- function(specifics, decorate = TRUE) {
  l <- length(specifics)

  if (l == 0) {
    return()
  }

  # get bullets
  ns <- names(specifics)

  # initialize bullets
  if (is.null(ns)) {
    ns <- rep("", l)
  }

  # add bullets
  for (i in 1:l) {
    n <- ns[i]

    # decorate bullets
    if (decorate) {
      bs <- getOption("erify.bullets")

      if (n %in% c("", "x")) {
        n <- bs$x
      } else if (n == "i") {
        n <- bs$i
      }

      n %<>% paste0(" ")
    }

    # prepend bullets
    specifics[i] %<>% paste0(n, .)
  }

  specifics %>% unname()
}


.Statement <- function(general = NULL, specifics = NULL, env = NULL,
                       decorate = TRUE, ...) {
  # normalize `specifics`
  specifics %<>% normalize_specifics(decorate)

  # create Statement object
  list(
    general = general,
    specifics = specifics,
    env = env,
    ...
  ) %>% `class<-`("Statement")
}


#' @export
print.Statement <- function(x, silent = FALSE, ...) {
  ss <- x$general

  specifics <- x$specifics
  if (length(specifics) != 0) {
    ss %<>% c(paste(specifics, collapse = "\n"))
  }

  s <- paste(ss, collapse = "\n\n")

  env <- x$env
  if (!is.null(env)) {
    s %<>% glue::glue(.envir = env)
  }

  .print_string(s, silent)
}


.shorten <- function(specifics, n = 5) {
  if (n == 0) {
    return()
  }

  l <- length(specifics)

  if (l <= n) {
    return(specifics)
  }

  specifics %<>% .[1:n]

  supplement <- ifelse(
    l == n + 1,
    "... and 1 more problem.",
    paste("... and", l - n, "more problems.")
  )

  c(specifics, supplement)
}



# trigger -----------------------------------------------------------------

is_empty <- function(statement) {
  statement %>%
    {length(.$general) == 0 && length(.$specifics) == 0}
}


.trigger <- function(statement, as = "error") {
  # do not trigger empty Statement
  if (is_empty(statement)) {
    return(invisible(NULL))
  }

  s <- print(statement, silent = TRUE)

  # prepare arguments for `rlang::abort()` family
  statement[c("general", "specifics", "env")] <- NULL
  args <- c(list(message = s), statement)

  f <- switch(
    as,
    "error" = rlang::abort,
    "warning" = rlang::warn,
    "message" = rlang::inform
  )

  do.call(f, args)
}



# validator utils ---------------------------------------------------------

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
    .join(valid)

  } else {
    v1 <- valid[1]
    v2 <- valid[2]
    ss <- character(0)

    if (!is.na(v1)) {
      ss %<>% c(glue::glue("larger than {v1}"))
    }

    if (!is.na(v2)) {
      ss %<>% c(glue::glue("smaller than {v2}"))
    }

    .join(ss, "and")
  }
}


phrase_valid_content <- function(valid, as_double) {
  if (as_double && is.integer(valid)) {
    valid %<>% as.double()
  }

  if (is.atomic(valid)) {
    valid %>%
      .back_quote(as_double) %>%
      .join()
  }
}



# validators --------------------------------------------------------------

.check_type <- function(x, valid, name = NULL, general = NULL,
                        specific = NULL, supplement = NULL, ...) {
  # extract feature
  feature <- typeof(x)

  # validity
  pass <- feature %in% valid

  # early return
  if (pass) {
    return(invisible(NULL))
  }

  # capture `name`
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  # general
  .general <- glue::glue("`{name}` must have type { .join(valid) }.")

  if (is.null(general)) {
    general <- .general
  }

  # specific
  if (is.null(specific)) {
    specific <- "`{name}` has type {feature}."
  }

  # add `supplement`
  specifics <- c(specific, supplement)

  .Statement(general, specifics, environment(), ...) %>% .trigger()
}


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
