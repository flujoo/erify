.check_type <- function(x, valid, name = NULL, general = NULL,
                        specifics = NULL, supplement = NULL, ...) {
  type <- typeof(x)

  if (type %in% valid) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  valid %<>% join()

  if (is.null(general)) {
    general <- "{name} must have type {valid}."
  }

  if (is.null(specifics)) {
    specifics = "{name} has type {type}."
  }

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


#' @rdname validators
#' @export
check_type <- function(x, valid, name = NULL, general = NULL,
                       specifics = NULL, supplement = NULL, ...) {
  .check_type(valid, "character")
  check_statement(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  .check_type(x, valid, name, general, specifics, supplement, ...)
}


#' @rdname validators
#' @export
check_types <- function(x, valid, name = NULL, general = NULL,
                        supplement = NULL, ...) {
  .check_type(x, "list")
  .check_type(valid, "character")
  check_statement(name, general, specifics = NULL, supplement)

  l <- length(x)

  if (l == 0) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("{name}")
  }

  specifics <- character(0)
  specific <- "`{name}[[{i}]]` has type {type}."

  for (i in 1:l) {
    x_i <- x[[i]]
    type <- typeof(x_i)

    if (!(type %in% valid)) {
      specifics <- c(specifics, glue(specific))
    }
  }

  if (length(specifics) == 0) {
    return(invisible(NULL))
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have type {s_valid}."
  }

  s_valid <- join(valid)

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


#' @rdname validators
#' @export
check_class <- function(x, valid, name = NULL, general = NULL,
                        specifics = NULL, supplement = NULL, ...) {
  .check_type(valid, "character")
  check_statement(name, general, specifics, supplement)

  if (inherits(x, valid)) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("`{name}`")
  }

  valid %<>% join()

  if (is.null(general)) {
    general <- "{name} must have class {valid}."
  }

  classes <- class(x)
  s_class <- ifelse(length(classes) == 1, "class", "classes")
  classes %<>% join("and")

  if (is.null(specifics)) {
    specifics = "{name} has {s_class} {classes}."
  }

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}


#' @rdname validators
#' @export
check_classes <- function(x, valid, name = NULL, general = NULL,
                          supplement = NULL, ...) {
  .check_type(x, "list")
  .check_type(valid, "character")
  check_statement(name, general, specifics = NULL, supplement)

  l <- length(x)

  if (l == 0) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
    name <- glue("{name}")
  }

  specifics <- character(0)
  specific <- "`{name}[[{i}]]` has class {c_}."

  for (i in 1:l) {
    x_i <- x[[i]]
    c_ <- class(x_i)

    if (!inherits(x_i, valid)) {
      specifics <- c(specifics, glue(specific))
    }
  }

  if (length(specifics) == 0) {
    return(invisible(NULL))
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have class {s_valid}."
  }

  s_valid <- join(valid)

  .Statement(general, specifics, supplement, env = environment(), ...) %>%
    .trigger()
}
