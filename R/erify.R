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


# shortcut to check commonly used arguments
check_arguments <- function(name = NULL, general = NULL, specifics = NULL,
                            supplement = NULL, specific = NULL, n = NULL) {
  if (!is.null(name)) {
    .check_string(name)
  }

  if (!is.null(general)) {
    .check_string(general)
  }

  if (!is.null(specifics)) {
    .check_type(specifics, "character")
  }

  if (!is.null(supplement)) {
    .check_string(supplement)
  }

  if (!is.null(specific)) {
    .check_string(specific)
  }

  if (!is.null(n)) {
    check_index(n)
  }
}
