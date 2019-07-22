## Test environments
* local Windows 10 install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)
* R-Hub (various)

## R CMD check results

0 errors | 0 warnings | 1 note

* This resubmission for a a new release.

* I have updated the DESCRIPTION formatting per CRAN maintainer request.
* This package is an API wrapper. The particular API requires users to use their own API key. I cannot run function examples or unit tests on CRAN, but all examples and unit tests run successfully in multiple other environments, on local and remote systems. Full test suite also runs on Travis-CI where I was able to import an encrypted key.
