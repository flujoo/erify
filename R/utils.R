#' @title Connect Words with Conjunction
#'
#' @description Connect given words with a conjunction, e.g. "and" and
#' "or".
#'
#' @param words A vector of list whose items can be converted to characters.
#'
#' @param conjunction A single character which represents a conjunction word.
#' The default value is `"or"`.
#'
#' @return If has length 1 or less, `words` is returned. Or items of `words`
#' are concatenated and returned.
#'
#' @export
#'
#' @examples
#' words <- c("apple", "orange", "Pink Floyd")
#' join(words, "and")
join <- function(words, conjunction = "or") {
  l <- length(words)

  if (l <= 1) {
    return(words)
  }

  paste(
    paste(words[-l], collapse = ", "),
    conjunction,
    words[l]
  )
}


#' @title Back Quote Object
#'
#' @description Convert an R object to character and add back quotations.
#'
#' @param x An R object.
#'
#' @param recursive Optional. `TRUE` or `FALSE` which indicates if to
#' back quote each item of `x` or to back quote `x` as a whole, when `x`
#' is a vector. The default value is `TRUE`.
#'
#' @param as_double Optional. `TRUE` or `FALSE` which indicates if to
#' differentiate between type double and integer. The default value is
#' `TRUE`, which means integers are handled as doubles.
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples
#' back_quote(1:3)
#'
#' back_quote(1:3, recursive = FALSE)
#'
#' back_quote(1:3, as_double = FALSE)
#'
#' back_quote(NULL)
#'
#' back_quote(list(c, 1:3, "a"))
back_quote <- function(x, recursive = TRUE, as_double = TRUE) {
  recursive <- recursive &&
    (is.atomic(x) || is.list(x)) &&
    length(x) > 0

  if (!recursive) {
    ss <- deparse(x)

  } else {
    if (as_double && is.integer(x)) {
      x <- as.double(x)
    }

    ss <- sapply(x, deparse, USE.NAMES = FALSE)
  }

  unclass(glue::glue("`{ss}`"))
}
