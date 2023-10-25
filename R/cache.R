#' Clear API results cache
#'
#' Reset the results of API calls that are currently cached in memory.
#'
#' `eia_clear_cache()` clears the entire cache. The other functions clear the
#' cache associated with specific endpoints.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' key <- Sys.getenv("EIA_KEY") # your stored API key
#' system.time(eia_dir(key))
#' system.time(eia_dir(key))
#' eia_clear_cache()
#' system.time(eia_dir(key))
#' }
eia_clear_cache <- function(){
  eia_clear_dir()
  eia_clear_metadata()
  eia_clear_data()
  eia_clear_facets()
  invisible()
}

#' @rdname eia_clear_cache
#' @export
eia_clear_dir <- function(){
  memoise::forget(.eia_dir_memoized)
  invisible()
}

#' @rdname eia_clear_cache
#' @export
eia_clear_metadata <- function(){
  memoise::forget(.eia_metadata_memoized)
  invisible()
}

#' @rdname eia_clear_cache
#' @export
eia_clear_data <- function(){
  memoise::forget(.eia_data_memoized)
  invisible()
}

#' @rdname eia_clear_cache
#' @export
eia_clear_facets <- function(){
  memoise::forget(.eia_facets_memoized)
  invisible()
}
