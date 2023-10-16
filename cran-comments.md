## Test environments

* Local Windows 10 install: R 4.3.1
* Win Builder: R-devel, R-release
* Ubuntu latest: R-devel, R-release, R-oldrel
* MacOS latest: R-release

## Update release

* This update includes a maintainer email address update.
* Refactored package to work with new version of the Energy Information Administration API.

## R CMD check results

0 errors | 0 warnings | 0 notes

Special note: This package is an API wrapper. The particular API requires users to use their own API key. I cannot run function examples or unit tests on CRAN, but all examples and unit tests run successfully in multiple other environments, on local and remote systems. Full test suite runs in GitHub workflows where I am able to import an encrypted key. API key-dependent vignettes are precompiled for CRAN.

## Reverse dependencies

All checks pass. (https://github.com/ropensci/eia/blob/master/revdep/)
