#' EIA facets
#'
#' Obtain facets for a given set of EIA data.
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
#' @param facet character
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization.
#' See details.
#' @param key API key: character if set explicitly; not needed if key is set
#' globally. See `eia_set_key()`.
#'
#' @return data frame, list, or character; see details.
#' @export
#'
#' @examples
#' \dontrun{
#' eia_facets("electricity/retail-sales", facet = "sectorid")
#' }
eia_facets <- function(dir, facet, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  if(cache) .eia_facets_memoized(dir, facet, tidy, key) else .eia_facets(dir, facet, tidy, key)
}

.eia_facets_url <- function(dir, facet, key){
  .eia_url(path = paste0(dir, "/facet/", facet, "/?api_key=", key))
}

.eia_facets <- function(dir, facet, tidy, key){
  r <- .eia_facets_url(dir, facet, key) |> .eia_get()
  if(is.na(tidy)) return(r)
  r <- jsonlite::fromJSON(r)
  if(!tidy) return(r)
  tibble::as_tibble(r$response$facets)
}

.eia_facets_memoized <- memoise::memoise(.eia_facets)
