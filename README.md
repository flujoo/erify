
# erify <img src="man/figures/logo.png" align="right" alt="logo" width="120"/>

<!-- badges: start -->
<!-- badges: end -->

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
#> Error in library(erify): there is no package called 'erify'
```

Check if the following argument is valid:

``` r
arg <- "I'm invalid."
```

For example, check if it has valid type:

``` r
check_type(arg, "integer")
#> Error in check_type(arg, "integer"): could not find function "check_type"
```

Check if it has valid length:

``` r
check_length(arg, 1)
#> Error in check_length(arg, 1): could not find function "check_length"
```

Or check if it is a positive integer:

``` r
check_n(arg)
#> Error in check_n(arg): could not find function "check_n"
```

## More

See `vignette("erify", package = "erify")` for a gentle introduction to
erify.
