.Statement <- function(general, specifics = NULL, supplement = NULL,
                       env = NULL, decorate = TRUE, ...) {
  specifics %<>% normalize_specifics(decorate)

  # create Statement object
  list(
    general = general,
    specifics = specifics,
    supplement = supplement,
    env = env,
    ...
  ) %>% `class<-`("Statement")
}


#' @export
print.Statement <- function(x, silent = FALSE, ...) {
  # convert each part
  ss <- x$general

  specifics <- x$specifics
  if (length(specifics) != 0) {
    ss %<>% c(paste(specifics, collapse = "\n"))
  }

  ss %<>% c(x$supplement)

  s <- paste(ss, collapse = "\n\n")

  # render `s`
  env <- x$env
  if (!is.null(env)) {
    s %<>% glue(env)
  }

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
      } else {
        n %<>% paste0(" ")
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


is_empty <- function(statement) {
  statement %>%
    {length(.$general) == 0 && length(.$specifics) == 0}
}


.trigger <- function(statement, as = "error", n = 5) {
  if (is_empty(statement)) {
    return(invisible(NULL))
  }

  if (as != "message") {
    statement %<>% shorten(n)
  }

  s <- print(statement, silent = TRUE)

  statement[c("general", "specifics", "supplement", "env")] <- NULL
  args <- c(list(message = s), statement)

  f <- switch(
    as,
    "error" = rlang::abort,
    "warning" = rlang::warn,
    "message" = rlang::inform
  )

  do.call(f, args)
}
