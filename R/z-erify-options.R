initialize_bullets <- function() {
  context <- where()

  if (context == "latex") {
    list(x = "* ", i = "* ")

  } else if (context %in% c("html", "docx", "rmd", "gfm")) {
    list(x = "\u2716 ", i = "\u2139 ")

  } else {
    list(
      x = "\u001b[0;31m\u2716\u001b[0m ",
      i = "\u001b[0;36m\u2139\u001b[0m "
    )
  }
}


erify_options <- list(
  erify.bullets = initialize_bullets()
)


# erify options that are already set will not be changed
.onLoad <- function(libname, pkgname) {
  to_set <- !(names(erify_options) %in% names(options()))

  if (any(to_set)) {
    options(erify_options[to_set])
  }

  invisible()
}


# erify options that are different from the default
# are assumed set by user, and thus will not be unset
.onUnload <- function(libpath) {
  # names of erify options to unset
  names_to_unset <- NULL

  for (name in names(erify_options)) {
    # get the value of `name` from `erify_options`
    default <- erify_options[[name]]

    # get the value of `name` from `options()`
    set <- getOption(name)

    # unset `name` only if `set` and `default` are identical
    if (identical(default, set)) {
      names_to_unset <- c(names_to_unset, name)
    }
  }

  # unset default erify options
  if (!is.null(names_to_unset)) {
    to_unset <- vector("list", length(names_to_unset))
    names(to_unset) <- names_to_unset
    options(to_unset)
  }

  invisible()
}
