#' @importFrom magrittr %>% %T>% %<>%
NULL


utils::globalVariables(".")


.onLoad <- function(libname, pkgname) {
  in_rmd <- is_rmd()

  if (in_rmd) {
    bullets <- list(x = "*", i = "*")
  } else {
    bullets <- list(
      x = "\u001b[0;31m\u2716\u001b[0m",
      i = "\u001b[0;36m\u2139\u001b[0m"
    )
  }

  prepend <- ifelse(
    in_rmd,
    "(erify)",
    "\u001b[1;31m(erify)\u001b[0m"
  )

  general <- paste(prepend, "{.general}")

  ops <- list(
    erify.bullets = bullets,
    erify.prepend = prepend,
    erify.general = general
  )

  options(ops)
}


.onUnload <- function(libpath) {
  ops <- list(
    erify.bullets = NULL,
    erify.prepend = NULL,
    erify.general = NULL
  )

  options(ops)
}
