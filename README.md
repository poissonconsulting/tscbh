
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/poissonconsulting/tscbh.svg?branch=master)](https://travis-ci.org/poissonconsulting/tscbh)
[![Coverage
status](https://codecov.io/gh/poissonconsulting/tscbh/branch/master/graph/badge.svg)](https://codecov.io/github/poissonconsulting/tscbh?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# tscbh

The goal of tscbh is to manage the [Columbia Basin Hydrological
Database](https://www.poissonconsulting.ca/data/2018/05/15/columbia-basin-hydrological-database.html).
The database is not provided with this package.

## Installation

To install the latest version from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

``` r
# install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("tscbh")
```

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/tscbh)

``` r
# install.packages("devtools")
devtools::install_github("poissonconsulting/tscbh")
```

## Citation

``` 

To cite package 'tscbh' in publications use:

  Joe Thorley (2018). tscbh: Columbia Basin Hydrological Database.
  R package version 0.0.0.9002.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {tscbh: Columbia Basin Hydrological Database},
    author = {Joe Thorley},
    year = {2018},
    note = {R package version 0.0.0.9002},
  }
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/tscbh/issues).

[Pull requests](https://github.com/poissonconsulting/tscbh/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its
terms.

# There is missing data in 2001 from Brilliant Dam from May 1 to November 1 inclusive. This hourly data has been searched for and is not present in digital format. This data has been replaced by the BBK data minus the HLK total discharge for each hour for this period.

# 

# Revelstoke was sent as just one discharge (REV) until Dec 1, 2009. After that point, it was sent as turbine (REVTB) and spill (REVS) discharge.

#
