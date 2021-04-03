.Specific <- function(specific, type = "error", bullet = NULL, ...) {
  # convert `type` to bullet
  if (is.null(bullet)) {
    bullet <- switch(
      type,
      "error" = "\033[0;31m✖\033[0m",
      "hint" = "\033[0;36mℹ\033[0m"
    )
  }

  # convert `...` to environment
  env <- list(...) %>% list2env()

  # create Specific object
  list(
    specific = specific,
    bullet = bullet,
    env = env
  ) %>% `class<-`("Specific")
}


#' @export
print.Specific <- function(x, silent = FALSE, ...) {
  s <-
    glue::glue(x$bullet, " ", x$specific, .envir = x$env) %>%
    unclass()

  if (silent) {
    s
  } else {
    cat(s, "\n")
    invisible(s)
  }
}
