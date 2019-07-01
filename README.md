
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eia <img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">

**Author:** [Matthew Leonawicz](https://leonawicz.github.io/blog/)
<a href="https://orcid.org/0000-0001-9452-2771" target="orcid.widget">
<image class="orcid" src="https://members.orcid.org/sites/default/files/vector_iD_icon.svg" height="16"></a>
<br/> **License:** [MIT](https://opensource.org/licenses/MIT)<br/>

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/leonawicz/eia?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/eia)
[![Travis build
status](https://travis-ci.org/leonawicz/eia.svg?branch=master)](https://travis-ci.org/leonawicz/eia)
[![Codecov test
coverage](https://codecov.io/gh/leonawicz/eia/branch/master/graph/badge.svg)](https://codecov.io/gh/leonawicz/eia?branch=master)

The `eia` package provides API access to data from the US [Energy
Information Administration](https://www.eia.gov/) (EIA).

## Installation

You can install the released version of `eia` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("eia")
```

You can install the development version of `eia` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("leonawicz/eia")
```

## Example

Load a time series of total electricity consumption:

``` r
library(eia)
key <- Sys.getenv("EIA_KEY")
id <- "ELEC.CONS_TOT_BTU.COW-AK-1.A"
(d <- eia_series(key, id, n = 10))
#> # A tibble: 1 x 13
#>   series_id name  units f     description copyright source iso3166
#>   <chr>     <chr> <chr> <chr> <chr>       <chr>     <chr>  <chr>  
#> 1 ELEC.CON~ Tota~ mill~ A     "Summation~ None      EIA, ~ USA-AK 
#> # ... with 5 more variables: geography <chr>, start <chr>, end <chr>,
#> #   updated <chr>, data <list>

d$data[[1]]
#> # A tibble: 10 x 2
#>     year value
#>    <int> <dbl>
#>  1  2018  7.35
#>  2  2017  5.95
#>  3  2016  6.21
#>  4  2015  7.58
#>  5  2014  6.53
#>  6  2013  2.59
#>  7  2012  2.92
#>  8  2011  2.42
#>  9  2010  2.61
#> 10  2009  2.88
```

See the package vignettes for more details and examples.
