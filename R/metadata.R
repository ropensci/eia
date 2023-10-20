#' EIA metadata
#'
#' Obtain EIA data metadata
#'
#' By default, additional processing is done to return a list containing tibble data frames.
#' Set `tidy = FALSE` to return only the initial list result of `jsonlite::fromJSON()`.
#' Set `tidy = NA` to return the original JSON as a character string.
#'
#' Set to `cache = FALSE` to force a new API call for updated data.
#' Using `FALSE` always makes a new API call and returns the result from the server.
#' `TRUE` uses memoization on a per R session basis, caching the result of the
#' function call in memory for the duration of the R session.
#' You can reset the entire cache by calling `eia_clear_cache()`.
#'
#' @param dir character, directory path.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization.
#' See details.
#' @param key API key: character if set explicitly; not needed if key is set
#' globally. See `eia_set_key()`.
#'
#' @return named list or character; see details.
#' @export
#'
#' @examples
#' \dontrun{
#' eia_dir("electricity/retail-sales")
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
    desc <- if(!is.null(r$response$description)) r$response$description
    data <- if(!is.null(r$response$data)) .eia_data_fmt(r$response$data)
    fcts <- if(!is.null(r$response$facets)) .eia_fcts_fmt(r$response$facets)
    freq <- if(!is.null(r$response$frequency)) .eia_freq_fmt(r$response$frequency)
    list(
      Name = name, Description = desc, Data = data, Facets = fcts, Frequency = freq,
      Defaults = tibble::tibble(
        format = r$response$defaultDateFormat,
        frequency = r$response$defaultFrequency
      ),
      Period = tibble::tibble(start = r$response$startPeriod, end = r$response$endPeriod)
    )
  }
}

.eia_metadata_memoized <- memoise::memoise(.eia_metadata)

.eia_metadata_url <- function(dir, key){
  .eia_url(path = paste0(dir, "/?api_key=", key))
}

.eia_data_fmt <- function(data){
  d <- sapply(names(data), function(x){
    unlist(list("id" = x, data[[x]]))
  })
  tibble::as_tibble(t(d))
}

.eia_fcts_fmt <- function(fcts){
  tibble::as_tibble(fcts)
}

.eia_freq_fmt <- function(freq){
  if ("format" %in% names(freq))
    freq$format <- gsub('"', "", freq$format)
  tibble::as_tibble(freq)
}
