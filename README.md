
# er <img src="man/figures/logo.png" align="right" alt="logo" width="120"/>

<!-- badges: start -->
<!-- badges: end -->

Check arguments, and generate readable error messages.

## Installation

Install er from CRAN:

``` r
install.packages("er")
```

Or install the development version from Github:

``` r
# install devtools if not
# install.packages("devtools")

devtools::install_github("flujoo/er")
```

## Example

Load er:

``` r
library(er)
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

See `vignette("er", package = "er")` for a gentle introduction to er.
