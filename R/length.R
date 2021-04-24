#' @rdname validators
#' @order 6
#' @export
check_length <- function(x, valid, interval = NULL, name = NULL,
                         general = NULL, specifics = NULL,
                         supplement = NULL, ...) {
  if (!is.null(interval)) {
    check_content(interval, c(TRUE, FALSE))
  }

  check_length_valid(valid, interval)
  check_statement(name, general, specifics, supplement)

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  .check_length(
    x, valid, interval, name, general, specifics, supplement, ...)
}


is_integer <- function(x) {
  con <- is.numeric(x) && all(as.integer(x) == x)
  ifelse(is.na(con), TRUE, con)
}


check_length_valid <- function(valid, interval) {
  #
  general <- "`valid` must be a numeric vector of non-negative integers."

  .check_type(valid, c("integer", "double"), general = general)
  .check_length(valid, c(0, NA), general = general)

  specific <- "valid[{i}] is {v}."
  specifics <- character(0)

  for (i in 1:length(valid)) {
    v <- valid[i]
    con <- is_integer(v) && (is.na(v) || v >= 0)

    if (!con) {
      specifics %<>% c(glue(specific))
    }
  }

  if (length(specifics) > 0) {
    .Statement(general, specifics) %>% .trigger()
  }

  #
  if (isFALSE(interval)) {
    if (all(is.na(valid))) {
      general <-
        "If `interval` is `FALSE`, `valid` must not contain only `NA`."
      specifics <-
        "`valid` is {valid}."

      valid %<>% as_code(env = list(x = valid))

      .Statement(general, specifics, env = environment()) %>%
        .trigger()
    }
  }

  if (isTRUE(interval)) {
    general <- "If `interval` is `TRUE`, `valid` must have length 2."
    .check_length(valid, 2, general = general)

    con <- valid[2] - 1 > valid[1]
    con <- ifelse(is.na(con), TRUE, con)

    if (con) {
      return(invisible(NULL))
    }

    general <- paste(
      "If `interval` is `TRUE`,",
      "`valid[2]` must be larger than `valid[1] + 1,",
      "if both are not `NA`."
    )

    specifics <-
      "`valid` is {valid}."

    valid %<>% as_code(env = list(x = valid))

    .Statement(general, specifics, env = environment()) %>%
      .trigger()
  }
}
