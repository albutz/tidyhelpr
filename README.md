
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyhelpr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/albutz/tidyhelpr/workflows/R-CMD-check/badge.svg)](https://github.com/albutz/tidyhelpr/actions)
<!-- badges: end -->

The tidyhelpr package is a conglomeration of useful helper function for
data wrangling with the tidyverse. It is (and per se will always be)
work in progress as I add a function if

1.  I construct roughly the same helper function in multiple
    (non-related) projects.
2.  The function is nontrivial.

At the moment, tidyhelpr includes the following functions:

-   `mutate_rows()` extends `dplyr::mutate()` by a predicate function
    parameter to apply the mutation only to rows that satisfy the
    predicate. Note that a similar task can be achieved by using
    `dplyr::if_else()` inside of `dplyr::mutate()`. However,
    `mutate_rows()` simplifies the code especially if multiple variables
    are wrangled.

-   `split_groups()` is a wrapper for `dplyr::group_split()` and names
    the list elements by the group keys. It allows for more than one
    group and concatenates the individual keys.

-   Ever had the struggle of reading in a csv file and realizing the
    relevant part starts only in line 1245? Then `read_sub()` is your
    friend: it lets you find the pattern for identifying the start
    point, splits lines and reads the relevant part via
    `readr::read_delim()`.

## Installation

You can install the development version of tidyhelpr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("albutz/tidyhelpr")
```

## Examples
