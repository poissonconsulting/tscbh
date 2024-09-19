
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/tscbh/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/tscbh/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/tscbh/graph/badge.svg)](https://app.codecov.io/gh/poissonconsulting/tscbh)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

# tscbh

The goal of tscbh is to manage the [Columbia Basin Hydrological
Database](https://www.poissonconsulting.ca/data/2018/05/15/columbia-basin-hydrological-database.html).
The database is not provided with this package.

## Installation

To install from GitHub

    install.packages("devtools")
    devtools::install_github("poissonconsulting/tscbh")

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/tscbh/issues).

[Pull requests](https://github.com/poissonconsulting/tscbh/pulls) are
always welcome.

## Code of Conduct

Please note that the tscbh project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## NOTES

There is missing data in 2001 from Brilliant Dam from May 1 to November
1 inclusive. This hourly data has been searched for and is not present
in digital format. This data has been replaced by the BBK data minus the
HLK total discharge for each hour for this period.

Revelstoke was sent as just one discharge (REV) until Dec 1, 2009. After
that point, it was sent as turbine (REVTB) and spill (REVS) discharge.
