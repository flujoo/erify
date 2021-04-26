#' @title Detect Where Code Is Running
#'
#' @description Check if code is running in RStudio, R Markdown file,
#' R Jupyter Notebook or other contexts. And if is in R Markdown file,
#' check the output format.
#'
#' @return For `where()`:
#'
#' - If executed in R Markdown file, it returns the output format.
#' If output format is not specified, it returns `"rmd"`.
#' - If executed in RStudio, it returns `"rstudio"`.
#' - If executed in R Jupyter Notebook, it returns `"jupyter"`.
#' - If executed in other contexts, it returns `"other"`.
#'
#' `is_rmd()`, `is_rstudio()` and `is_jupyter()` return `TRUE` if executed
#' in their corresponding contexts, or `FALSE` if not.
#'
#' @seealso [rstudioapi::isAvailable()], [knitr::pandoc_to()].
#'
#' @export
where <- function() {
  if (is_rmd()) {
    to <- knitr::pandoc_to()
    ifelse(is.null(to), "rmd", to)

  } else if (rstudioapi::isAvailable()) {
    "rstudio"

  } else if (is_jupyter()) {
    "jupyter"

  } else {
    "other"
  }
}


#' @rdname where
#' @export
is_rmd <- function() {
  getOption('knitr.in.progress') %>%
    isTRUE()
}


#' @rdname where
#' @export
is_rstudio <- function() {
  !is_rmd() && rstudioapi::isAvailable()
}


#' @rdname where
#' @export
is_jupyter <- function() {
  options() %>%
    names() %>%
    grep("jupyter", .) %>%
    length() %>%
    as.logical()
}
