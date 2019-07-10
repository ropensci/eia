## Test environments
* local Windows 10 install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* This package is an API wrapper. The API requires users to use their own API key. I cannot run function examples or unit tests or build vignettes on CRAN without a key, but all examples and unit tests run successfully in multiple other environments, on local and remote systems.
* For purposes of CRAN submission, I have had to exclude vignettes from the package and make the vignettes available only on the package's external website. Please advise if there is an alternative way to include vignettes, but since I cannot publish an API key my impression is the vignettes must be excluded from package build in `.Rbuildignore`. All six online vignettes can be found here: https://leonawicz.github.io/eia/articles/eia.html
