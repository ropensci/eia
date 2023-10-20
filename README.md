
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eia <img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![R-CMD-check](https://github.com/ropensci/eia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/eia/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/eia/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ropensci/eia?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/eia)](https://cran.r-project.org/package=eia)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/eia)](https://cran.r-project.org/package=eia)
[![Github
Stars](https://img.shields.io/github/stars/ropensci/eia.svg?style=social&label=Github)](https://github.com/ropensci/eia)
<!-- badges: end -->

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
the package. You can set it with `eia_set_key()` in your R session. You
can always pass it explicitly to the `key` argument of a function.

``` r
library(eia)

# not run
eia_set_key("yourkey") # set API key if not already set globally
```

Get a list of available data categories provided by the EIA’s API.

``` r
eia_dir()
#> # A tibble: 14 × 3
#>    id                name                            description                
#>    <chr>             <chr>                           <chr>                      
#>  1 coal              Coal                            EIA coal energy data       
#>  2 crude-oil-imports Crude Oil Imports               Crude oil imports by count…
#>  3 electricity       Electricity                     EIA electricity survey data
#>  4 international     International                   Country level production, …
#>  5 natural-gas       Natural Gas                     EIA natural gas survey data
#>  6 nuclear-outages   Nuclear Outages                 EIA nuclear outages survey…
#>  7 petroleum         Petroleum                       EIA petroleum gas survey d…
#>  8 seds              State Energy Data System (SEDS) Estimated production, cons…
#>  9 steo              Short Term Energy Outlook       Monthly short term (18 mon…
#> 10 densified-biomass Densified Biomass               EIA densified biomass data 
#> 11 total-energy      Total Energy                    These data represent the m…
#> 12 aeo               Annual Energy Outlook           Annual U.S. projections us…
#> 13 ieo               International Energy Outlook    Annual international proje…
#> 14 co2-emissions     State CO2 Emissions             EIA CO2 Emissions data

eia_dir("electricity")
#> # A tibble: 6 × 3
#>   id                              name                 description              
#>   <chr>                           <chr>                <chr>                    
#> 1 retail-sales                    Electricity Sales t… "Electricity sales to ul…
#> 2 electric-power-operational-data Electric Power Oper… "Monthly and annual elec…
#> 3 rto                             Electric Power Oper… "Hourly and daily electr…
#> 4 state-electricity-profiles      State Specific Data  "State Specific Data"    
#> 5 operating-generator-capacity    Inventory of Operab… "Inventory of operable g…
#> 6 facility-fuel                   Electric Power Oper… "Annual and monthly elec…
```

## References

See the collection of vignette tutorials and examples as well as
complete package documentation available at the `eia` package
[website](https://docs.ropensci.org/eia/).

------------------------------------------------------------------------

Please note that the `eia` project is released with a [Contributor Code
of
Conduct](https://github.com/ropensci/eia/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
