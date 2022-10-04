
# erify <img src="man/figures/logo.png" align="right" alt="logo" width="120"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Check arguments and generate readable error messages.

## Motivation

When creating functions for other people to use, you always need to

1.  check if the arguments passed by users are valid, and if not,
2.  generate informative and well-formatted error messages in a
    consistent style.

erify serves the exact purpose.

## Installation

Install erify from CRAN:

``` r
install.packages("erify")
```

Or install the development version from Github:

``` r
# install devtools if not
# install.packages("devtools")

devtools::install_github("flujoo/erify")
```

## Example

Suppose you are creating a function which prints a string several times
to emphasize it:

``` r
# print `what` `n` times
emphasize <- function(what, n) {
  for (i in 1:n) {
    cat(what, "\n")
  }
}

# example
emphasize("You're beautiful!", 3)
#> You're beautiful! 
#> You're beautiful! 
#> You're beautiful!
```

And suppose a novice user accidentally passes a function to argument
`what`, he/she will get an error message which is not very readable:

``` r
emphasize(c, 3)
#> Error in cat(what, "\n"): argument 1 (type 'builtin') cannot be handled by 'cat'
```

You can improve this by adding erify’s `check_type()` into
`emphasize()`:

``` r
emphasize <- function(what, n) {
  # check the type of `what`
  erify::check_type(what, "character")
  
  # main
  for (i in 1:n) {
    cat(what, "\n")
  }
}

emphasize(c, 3)
#> Error: `what` must have type character.
#> 
#> ✖ `what` has type builtin.
```

In the above code, `check_type(what, "character")` checks if `what` has
type character, and if not, generates improved error message.

## More

You can add more functions to check arguments, customize error messages,
and create your own check functions.

See `vignette("erify")` for a gentle introduction to erify.
