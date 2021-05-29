throw <- function(general, specifics = NULL, env = NULL, as = "error",
                  class = NULL, ...) {
  # return silently if `specifics` is empty
  # this is useful in checking items of vectors
  pass <- !is.null(specifics) && length(specifics) == 0

  if (pass) {
    return(invisible())
  }

  to_string(general, specifics, env, getOption("erify.n")) |>
    generate_condition(as, class, ...) |>
    trigger(as)
}


# prepend bullet to each item of `specifics`
decorate_specifics <- function(specifics) {
  bullets <- getOption("erify.bullets")

  for (i in seq_along(specifics)) {
    specific <- specifics[i]
    name <- names(specific)

    # prepend bullet
    if (is.null(name) || name %in% c("", "x")) {
      bullet <- bullets$x
    } else if (name == "i") {
      bullet <- bullets$i
    } else {
      bullet <- name
    }

    specifics[i] <- paste0(bullet, specific)
  }

  specifics
}


# shorten `specifics` if its length is larger than `n`
shorten <- function(specifics, n = 5L) {
  if (n == 0) {
    return()
  }

  l <- length(specifics)

  if (l <= n) {
    return(specifics)
  }

  specifics <- specifics[1:n]

  supplement <- ifelse(
    l == n + 1,
    "... and 1 more problem.",
    paste("... and", l - n, "more problems.")
  )

  c(specifics, supplement)
}


# combine `general` and `specifics` to a printable string
to_string <- function(general, specifics = NULL, env = NULL, n = 5L) {
  if (!is.null(specifics)) {
    specifics <- specifics |>
      shorten(n) |>
      decorate_specifics() |>
      paste(collapse = "\n")

    # combine `general` and `specifics`
    general <- paste(general, specifics, sep = "\n\n")
  }

  if (!is.null(env)) {
    general <- glue::glue(general, .envir = env)
  }

  unclass(general)
}


# check `?conditions`
generate_condition <- function(message, as = "error", class = NULL, ...) {
  classes <- c(class, as, "condition")

  list(
    message = message,
    ...
  ) |> `class<-`(classes)
}


# trigger `condition`
trigger <- function(condition, as = "error") {
  if (as == "error") {
    stop(condition)

  } else if (as == "warning") {
    warning(condition)

  } else if (as == "message") {
    message(condition)
  }
}
