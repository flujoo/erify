.Statement <- function(general, specifics = NULL, supplement = NULL,
                       class = NULL, ...) {
  # convert `...` to environment
  env <- list(...) %>% list2env()

  if (!is.null(specifics)) {
    specifics %<>% normalize_specifics()
  }

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
    ss <- specifics %>%
      sapply(print, silent = TRUE) %>%
      paste(collapse = "\n") %>%
      c(ss, .)
  }

  # convert `$supplement`
  supplement <- x$supplement
  if (!is.null(supplement)) {
    ss %<>% c(supplement)
  }

  s <- ss %>%
    paste(collapse = "\n\n") %>%
    glue::glue(.envir = x$env) %>%
    unclass()

  if (silent) {
    s
  } else {
    cat(s, "\n")
    invisible(s)
  }
}


normalize_specifics <- function(specifics) {
  # names are types or bullets
  ns <- names(specifics)
  ss <- list()

  for (i in 1:length(specifics)) {
    s <- specifics[[i]]

    if (!inherits(s, "Specific")) {
      if (is.null(ns)) {
        s %<>% .Specific()

      } else {
        n <- ns[i]

        if (n == "") {
          s %<>% .Specific()
        } else if (n %in% c("error", "hint")) {
          s %<>% .Specific(type = n)
        } else {
          s %<>% .Specific(bullet = n)
        }
      }
    }

    ss %<>% c(list(s))
  }

  ss
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

  statement$supplement %<>% paste(supplement, "\n\n", .)

  statement
}


.trigger <- function(statement, as = "error", n = 5, ...) {
  statement %<>% shorten(n)
  s <- print(statement, silent = TRUE)

  f <- switch(
    as,
    "error" = rlang::abort,
    "warning" = rlang::warn,
    "message" = rlang::inform
  )

  f(s, class = statement$class, ...)
}
