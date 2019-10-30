
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eia <img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">

**Author:** [Matthew Leonawicz](https://github.com/leonawicz)
<a href="https://orcid.org/0000-0001-9452-2771" target="orcid.widget">
<image class="orcid" src="https://members.orcid.org/sites/default/files/vector_iD_icon.svg" height="16"></a>
<br/> **License:** [MIT](https://opensource.org/licenses/MIT)<br/>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis build
status](https://travis-ci.org/ropensci/eia.svg?branch=master)](https://travis-ci.org/ropensci/eia)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/ropensci/eia?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/eia)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/eia/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/eia?branch=master)

[![](https://badges.ropensci.org/342_status.svg)](https://github.com/ropensci/onboarding/issues/342)
[![CRAN
status](http://www.r-pkg.org/badges/version/eia)](https://cran.r-project.org/package=eia)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/eia)](https://cran.r-project.org/package=eia)
[![Github
Stars](https://img.shields.io/github/stars/ropensci/eia.svg?style=social&label=Github)](https://github.com/ropensci/eia)

The `eia` package provides API access to data from the US [Energy
Information Administration](https://www.eia.gov/) (EIA).

Pulling data from the US Energy Information Administration (EIA) API
requires a registered API key. A key can be obtained at no cost
[here](https://www.eia.gov/opendata/register.php). A valid email and
agreement to the API Terms of Service is required to obtain a key.

`eia` includes functions for searching EIA API data categories and
importing time series and geoset time series datasets. Datasets returned
by these functions are provided in a tidy format or alternatively in
more raw form. It also offers helper functions for working with EIA API
date strings and time formats and for inspecting different summaries of
series metadata. The package also provides control over API key storage
and caching of API request results.

## Installation

Install the CRAN release of `eia` with

``` r
install.packages("eia")
```

To install the development version from GitHub use

``` r
# install.packages("remotes")
remotes::install_github("ropensci/eia")
```

## Example

To begin, store your API key. You can place it somewhere like your
`.Renviron` file and never have to do anything with the key when you use
the package. You can set it with `eia_set_key` in your R session. You
can always pass it explicitly to the `key` argument of a function.

``` r
library(eia)

# not run
eia_set_key("yourkey") # set API key if not already set globally
```

Load a time series of net electricity generation.

``` r
id <- "ELEC.GEN.ALL-AK-99.A"
(d <- eia_series(id, n = 10))
#> # A tibble: 1 x 13
#>   series_id name  units f     description copyright source iso3166
#>   <chr>     <chr> <chr> <chr> <chr>       <chr>     <chr>  <chr>  
#> 1 ELEC.GEN~ Net ~ thou~ A     "Summation~ None      EIA, ~ USA-AK 
#> # ... with 5 more variables: geography <chr>, start <chr>, end <chr>,
#> #   updated <chr>, data <list>

d$data[[1]]
#> # A tibble: 10 x 3
#>    value date        year
#>    <dbl> <date>     <int>
#>  1 6247. 2018-01-01  2018
#>  2 6497. 2017-01-01  2017
#>  3 6335. 2016-01-01  2016
#>  4 6285. 2015-01-01  2015
#>  5 6043. 2014-01-01  2014
#>  6 6497. 2013-01-01  2013
#>  7 6946. 2012-01-01  2012
#>  8 6871. 2011-01-01  2011
#>  9 6760. 2010-01-01  2010
#> 10 6702. 2009-01-01  2009

library(ggplot2)
library(tidyr)
unnest(d, cols = data) %>% ggplot(aes(year, value)) +
  geom_line() + labs(y = d$units, title = d$name)
```

<img src="man/figures/README-example-1.png" width="100%" />

## References

See the collection of vignette tutorials and examples as well as
complete package documentation available at the `eia` package
[website](https://docs.ropensci.org/eia).

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/ropensci/eia/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
