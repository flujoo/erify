
# erify <img src="man/figures/logo.png" align="right" alt="logo" width="120"/>

Check arguments, and generate readable error messages.

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

Load erify:

``` r
library(erify)
```

Check if the following argument is valid:

``` r
arg <- "I'm invalid."
```

For example, check if it has valid type:

``` r
check_type(arg, "integer")
#> Error: `arg` must have type integer.
#> 
#> ✖ `arg` has type character.
```

Check if it has valid length:

``` r
check_length(arg, 1)
```

Or check if it is a positive integer:

``` r
check_n(arg)
#> Error: `arg` must be a single positive integer.
#> 
#> ✖ `arg` has type character.
```

## More

See `vignette("erify", package = "erify")` for a gentle introduction to
erify.
