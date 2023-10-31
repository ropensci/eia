## Test environments

* Local Windows 10 install: R 4.3.1
* Win Builder: R-devel, R-release
* Windows latest: R-release
* Ubuntu latest: R-devel, R-release, R-oldrel
* MacOS latest: R-release

## Update release

* This update includes a maintainer change/email address update.
* Refactored package to work with newer version 2 of the US Energy Information Administration API.

## R CMD check results

0 errors | 0 warnings | 0 notes

WinBuilder NOTE: WinBuilder flags three URLs for the 'US Energy Information Administration' and 'ROpenSci' as possibly invalid but they are all correct.

Special note: This package is an API wrapper. The particular API requires users to use their own API key. We cannot run function examples or unit tests on CRAN, but all examples and unit tests run successfully in multiple other environments, on local and remote systems, noted above. Full test suite runs in GitHub workflows and elsewhere, where an imported key can be used. API key-dependent vignettes are precompiled for CRAN.

Further, there are 3 NOTEs produced by `devtools::check_rhub()`:

1. Relating to the change in Maintainer:
```
Maintainer: 'Matthew Hoff <matthew.g.hoff@gmail.com>'
  
  New maintainer:
    Matthew Hoff <matthew.g.hoff@gmail.com>
  Old maintainer(s):
    Matthew Leonawicz <mfleonawicz@gmail.com>
```

The next two are found only on Windows Server 2022, R-devel, 64 bit:

2. Relating to the potential of a bug/crash in MiKTeX, as described in
[R-hub issue #503](https://github.com/r-hub/rhub/issues/503), can likely be ignored.
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

3. Relating to known [R-hub issue #560](https://github.com/r-hub/rhub/issues/560)
can likely be ignored.
```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```

## Reverse dependencies

All checks pass. (https://github.com/ropensci/eia/blob/master/revdep/)
