
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LinAlg4

The goal of LinAlg4 is to make the life easier for students taking
advance cources in Linear Algebra.

## Installation

You can install the released version of LinAlg4 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("LinAlg4")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lucasbagge/LinAlg4")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LinAlg4)
string <- c(1,1,1,1,1,1)

A <- matrix_create(string, 3, 3)

A
#>      [,1] [,2] [,3]
#> [1,]    1    1    1
#> [2,]    1    1    1
#> [3,]    1    1    1
```
