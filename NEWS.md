# erify 0.3.0

* Add `is_n()`.
* Add `check_contents()`.
* Add `throw()`.
* Deprecate `print_string()`.
* Deprecate `Statement()` and `trigger()`, please use `throw()`.
* Remove dependencies on package magrittr and rlang.
* Add options `erify.bullets` and `erify.n`.
* Require R >= 4.1.0


# erify 0.2.0

## Features

* Add `check_binary_classes()`.
* Add `where()` and related functions for detecting where code is running.
* Add `join()` and `back_quote()` to facilitate error message generation.

## Changes

* Add `n` and `specific` in `check_classes()` and `check_types()`.
* Replace `specifics` with `specific` in validator functions.
* Deprecate `n` in `trigger()`.
* Deprecate `supplement` in `Statement()`.

## Documentation

* Revise README and vignette.
* Elaborate documentation.
