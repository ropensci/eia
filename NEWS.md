# eia 0.3.6

* Switch from http to https.

# eia 0.3.5

* Minor documentation updates.

# eia 0.3.4

* Added initial report function.
* Minor code improvements.
* Minor documentation updates.

# eia 0.3.3

* Added a wrapper for the series category endpoint.
* Updated documentation, vignette and unit tests.

# eia 0.3.2

* Updated formatting for CRAN.

# eia 0.3.1

* Updated package metadata and improved Travis testing suite configuration.

# eia 0.3.0

* Added convenient key store methods with getter and setter helpers, optionally making it easy to avoid having to provide the key in every function call.
* Moved `key` argument from first to last among relevant function arguments and updated all examples accordingly.
* Added support for hourly time series requests and date format handling.

# eia 0.2.0

* Added optional memoization to API functions, adding a new `cache` argument.
* Added anti-DOS measures, which can be adjusted using `options()`.
* Added more vignettes and documentation.
* Added helper functions for clearing cached results.
* Added helper functions for working with EIA date strings.
* Added helper functions for time series metadata.
* More output formats and consistency between functions.
* Added unit tests.
* Minor updates to functions, documentation.

# eia 0.1.0

* Added package scaffolding.
* Added initial package functions for working with data categories, time series, and geosets.
* Added function documentation, unit tests, vignettes.
