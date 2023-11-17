## Test environments

* Local Windows 10 install: R 4.3.1
* Win Builder: R-devel, R-release
* Windows latest: R-release
* Ubuntu latest: R-devel, R-release, R-oldrel
* MacOS latest: R-release

## Update release

* Code updates and additional unit testing.

## R CMD check results

0 errors | 0 warnings | 0 notes

WinBuilder NOTE: WinBuilder flags three URLs for the 'US Energy Information Administration' and 'ROpenSci' as possibly invalid but they are all correct.

Special note: This package is an API wrapper. The particular API requires users to use their own API key. We cannot run function examples or unit tests on CRAN, but all examples and unit tests run successfully in multiple other environments, on local and remote systems, noted above. Full test suite runs in GitHub workflows and elsewhere, where an imported key can be used. API key-dependent vignettes are precompiled for CRAN.

## Reverse dependencies

All checks pass. (https://github.com/ropensci/eia/blob/master/revdep/)
