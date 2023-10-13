#' EIA metadata
#'
#' Obtain EIA data metadata
#'
#' By default, additional processing is done to return a list containing tibble data frames.
#' Set `tidy = FALSE` to return only the initial list result of `jsonlite::fromJSON`.
#' Set `tidy = NA` to return the original JSON as a character string.
#'
#' Set to `cache = FALSE` to force a new API call for updated data.
#' Using `FALSE` always makes a new API call and returns the result from the server.
#' `TRUE` uses memoization on a per R session basis, caching the result of the
#' function call in memory for the duration of the R session.
#' You can reset the entire cache by calling `eia_clear_cache`.
#'
#' @param dir character, directory path.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization. See details.
#' @param key API key: character if set explicitly; not needed if key is set globally. See `eia_set_key()`.
#'
#' @return if `tidy = TRUE`, then `NULL`; if `tidy = NA` or `tidy = FALSE`, then
#' JSON the latter of which is a list result from `jsonlite::fromJSON`.
#' @export
#'
#' @examples
#' \dontrun{
#' eia_directory("electricity/retail-sales")
#' eia_metadata("electricity/retail-sales")
#' }
eia_metadata <- function(dir, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  if(cache) .eia_metadata_memoized(dir, tidy, key) else .eia_metadata(dir, tidy, key)
}

.eia_metadata <- function(dir, tidy, key){
  r <- .eia_metadata_url(dir, key) |> .eia_get()
  if(is.na(tidy)) return(r)
  r <- jsonlite::fromJSON(r)
  if(!tidy) return(r)
  if(tidy){
    name <- if(!is.null(r$response$name)) r$response$name else r$response$id
    cat("Name:\n  ", name)
    if(!is.null(r$response$description)){
      cat("\n\nDescription:\n  ", r$response$description)
    }
    if (!is.null(r$response$data)){
      cat("\n\nData Values:\n  "); print(.eia_data_values(r$response$data))
    }
    if (!is.null(r$response$facets)){
      cat("\nFacets:\n  "); print(tibble::as_tibble(r$response$facets))
    }
    if (!is.null(r$response$frequency)){
      cat("\nFrequency:\n  "); print(.eia_freq_fmt(r$response$frequency))
    }
    cat("\nDefaults:\n  ")
    cat("Date Format:", r$response$defaultDateFormat, "\n  Frequency:", r$response$defaultFrequency)
    cat("\n\nDate Range: ", r$response$startPeriod, "to", r$response$endPeriod)
    invisible(NULL)
  }
}

.eia_metadata_memoized <- memoise::memoise(.eia_metadata)

.eia_metadata_url <- function(dir, key){
  .eia_url(path = paste0(dir, "/?api_key=", key))
}

.eia_data_values <- function(data){
  d <- sapply(names(data), function(x){
    unlist(list("id" = x, data[[x]]))
  })
  tibble::as_tibble(t(d))
}

.eia_freq_fmt <- function(freq){
  if ("format" %in% names(freq))
    freq$format <- gsub('"', "", freq$format)
  tibble::as_tibble(freq)
}
