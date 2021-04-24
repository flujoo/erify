#' @importFrom magrittr %>% %T>% %<>%
NULL


utils::globalVariables(".")


# shortcut to check commonly used arguments
check_arguments <- function(name = NULL, general = NULL, specifics = NULL,
                            specific = NULL, n = NULL) {
  if (!is.null(name)) {
    .check_string(name)
  }

  if (!is.null(general)) {
    .check_string(general)
  }

  if (!is.null(specifics)) {
    .check_type(specifics, "character")
  }

  if (!is.null(specific)) {
    .check_string(specific)
  }

  if (!is.null(n)) {
    check_index(n)
  }
}
