## Test environments

* local Windows 10 install, R 3.6.1
* Windows 10 (AppVeyor), R 3.6.1
* Ubuntu 16.04 (Travis CI), R-devel, R-release (3.6.1), R-oldrel (3.5.3)
* Mac OSX (Travis CI) R-release (3.6.1)
* win-builder (devel and release)
* R-hub (various)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is an update release.

* Added minor functionality.
* Precompiling vignettes dependent on external data and user-specific API key.
* Updated files to reflect transfer of GitHub repository to ROpenSci.

Special note: This package is an API wrapper. The particular API requires users to use their own API key. I cannot run function examples or unit tests on CRAN, but all examples and unit tests run successfully in multiple other environments, on local and remote systems. Full test suite also runs on Travis-CI where I am able to import an encrypted key. API key-dependent vignettes are precompiled for CRAN.
