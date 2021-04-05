#' @importFrom magrittr %>% %T>% %<>%
NULL


utils::globalVariables(".")


join <- function(words, conjunction = "or") {
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


glue <- function(x, env = parent.frame()) {
  x %>%
    glue::glue(.envir = env) %>%
    unclass()
}
