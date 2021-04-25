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

  if (l == 1) {
    return(words)
  }

  paste(
    paste(words[-l], collapse = ", "),
    conjunction,
    words[l]
  )
}


# shortcut to check commonly used arguments
check_arguments <- function(name = NULL, general = NULL, specific = NULL,
                            supplement = NULL, n = NULL) {
  .general <- getOption("erify.general")

  if (!is.null(name)) {
    .check_string(name, general = .general)
  }

  if (!is.null(general)) {
    .check_string(general, general = .general)
  }

  if (!is.null(specific)) {
    .check_string(specific, general = .general)
  }

  if (!is.null(supplement)) {
    .check_type(supplement, "character", general = .general)
  }

  if (!is.null(n)) {
    check_index(n, general = .general)
  }
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



# length utils ------------------------------------------------------------

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



# validators --------------------------------------------------------------

.check_type <- function(x, valid, name = NULL, general = NULL,
                        specific = NULL, supplement = NULL, feature = NULL,
                        ...) {
  # extract feature
  if (is.null(feature)) {
    feature <- typeof(x)
  }

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
                          feature = NULL, ...) {
  # normalize `interval`
  interval %<>% normalize_interval(valid)

  # extract feature
  if (is.null(feature)) {
    feature <- length(x)
  }

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
  .general <-
    glue::glue("`{name}` must have length { phrase_valid_length(valid) }.")

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
}
