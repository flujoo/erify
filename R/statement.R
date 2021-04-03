.Statement <- function(general, specifics = NULL, supplement = NULL,
                       class = NULL, env = NULL, decorate = TRUE) {
  specifics %<>% normalize_specifics(decorate)

  # create Statement object
  list(
    general = general,
    specifics = specifics,
    supplement = supplement,
    class = class,
    env = env
  ) %>% `class<-`("Statement")
}


#' @export
print.Statement <- function(x, silent = FALSE, ...) {
  # strings of parts
  ss <- character(0)

  # convert `$general`
  general <- x$general
  if (!is.null(general)) {
    ss %<>% c(general)
  }

  # convert `$specifics`
  specifics <- x$specifics
  if (!is.null(specifics)) {
    ss %<>% c(paste(specifics, collapse = "\n"))
  }

  # convert `$supplement`
  supplement <- x$supplement
  if (!is.null(supplement)) {
    ss %<>% c(supplement)
  }

  s <- ss %>%
    paste(collapse = "\n\n") %>%
    glue(env = parent.frame())

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

  ns <- names(specifics)

  if (is.null(ns)) {
    ns <- rep("", l)
  }

  for (i in 1:l) {
    n <- ns[i]

    if (decorate) {
      if (n %in% c("", "x")) {
        n <- "\033[0;31m✖\033[0m "
      } else if (n == "i") {
        n <- "\033[0;36mℹ\033[0m "
      }
    }

    specifics[i] %<>% paste0(n, .)
  }

  specifics %>% unname()
}


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

  statement$supplement %<>% paste0(supplement, "\n\n", .)

  statement
}


.trigger <- function(statement, as = "error", n = 5, ...) {
  if (as != "message") {
    statement %<>% shorten(n)
  }

  s <- print(statement, silent = TRUE)

  f <- switch(
    as,
    "error" = rlang::abort,
    "warning" = rlang::warn,
    "message" = rlang::inform
  )

  f(s, class = statement$class, ...)
}
