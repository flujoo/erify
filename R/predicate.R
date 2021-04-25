is_integer <- function(x) {
  all(
    is.numeric(x),
    is.finite(x), # also excludes `NA`
    all(as.integer(x) == x)
  )
}
