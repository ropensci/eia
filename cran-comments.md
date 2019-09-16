## Test environments
* local Windows 10 install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)
* R-Hub (various)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is an update release.

* Added wrapper function for another API endpoint.
* Updated documentation, vignettes and unit tests.

Special note: This package is an API wrapper. The particular API requires users to use their own API key. I cannot run function examples or unit tests on CRAN, but all examples and unit tests run successfully in multiple other environments, on local and remote systems. Full test suite also runs on Travis-CI where I am able to import an encrypted key.
