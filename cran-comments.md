## Test environments

* Local Windows 10 install: R 4.3.3
* Win Builder: R-devel, R-release
* Windows latest: R-release
* Ubuntu latest: R-devel, R-release, R-oldrel
* MacOS latest: R-release

## Update release

* Fixed bug relating to spaces in provided facet ids - replaced with '%20' for valid URL request.
* Fixed bug relating to changes in the EIA API that now wraps all bad requests in an initial HTTP 500 error code which returns a JSON body containing the true HTTP error code.

## R CMD check results

0 errors | 0 warnings | 0 notes

WinBuilder NOTE: WinBuilder flags three URLs for the 'US Energy Information Administration' and 'ROpenSci' as possibly invalid but they are all correct.

Special note: This package is an API wrapper. The particular API requires users to use their own API key. We cannot run function examples or unit tests on CRAN, but all examples and unit tests run successfully in multiple other environments, on local and remote systems, noted above. Full test suite runs in GitHub workflows and elsewhere, where an imported key can be used. API key-dependent vignettes are precompiled for CRAN.

## Reverse dependencies

All checks pass. (https://github.com/ropensci/eia/blob/master/revdep/)
