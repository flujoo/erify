is_integer <- function(x, na = NULL) {
  if (!is.null(na)) {
    # check_bool(na)
  }

  if (is.null(na)) {
    na <- TRUE
  }

  con <- is.numeric(x) && all(as.integer(x) == x)
  ifelse(is.na(con), na, con)
}
