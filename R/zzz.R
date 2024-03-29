initialize_bullets <- function() {
  # check if the code is executed in an R Markdown document
  in_rmd <- isTRUE(getOption("knitr.in.progress")) &&
    requireNamespace("knitr", quietly = TRUE)

  if (in_rmd) {
    # check if the document is knitted to a latex file
    if (knitr::pandoc_to() == "latex") {
      list(x = "*", i = "*")
    } else {
      list(x = "\u2716", i = "\u2139")
    }
  } else {
    list(
      x = "\u001b[0;31m\u2716\u001b[0m",
      i = "\u001b[0;36m\u2139\u001b[0m"
    )
  }
}


# erify options that are already set will not be changed
.onLoad <- function(libname, pkgname) {
  ops <- list(
    erify.bullets = initialize_bullets(),
    # how many specifics at most to display
    erify.n = 5L
  )

  to_set <- !(names(ops) %in% names(options()))

  if (any(to_set)) {
    options(ops[to_set])
  }

  invisible()
}


.onUnload <- function(libpath) {
  ops <- list(
    erify.bullets = NULL,
    erify.n = NULL
  )

  options(ops)
  invisible()
}
