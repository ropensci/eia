#' EIA directory
#'
#' Obtain EIA directory listing.
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
#' @param dir character, directory path, if `NULL` then the API root directory.
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
#' # use eia_set_key() to store API key
#' eia_dir()
#' eia_dir("electricity")
#' eia_dir("electricity/rto")
#' }
eia_dir <- function(dir = NULL, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  if(cache) .eia_dir_memoized(dir, tidy, key) else .eia_dir(dir, tidy, key)
}

.eia_dir <- function(dir, tidy, key){
  spltdir <- if (!is.null(dir) && grepl("/", dir))
    unlist(strsplit(dir, "/"))
  r <- .eia_dir_url(dir, key) |> .eia_get()
  if(is.na(tidy)) return(r)
  r <- jsonlite::fromJSON(r)
  if(!tidy) return(r)
  if (!is.null(r$response$routes)){
    r <- r$response$routes
  } else {
    message(
      "No further sub-directories to discover.\n",
      "Use `eia_metadata('", dir, "')` to explore this data."
    )
  }
  if(tidy && is.data.frame(r))
    tibble::as_tibble(sapply(r, function(x) { gsub("( \\r\\n) *", " ", x) }))

  ## NOT SURE OF USEFULNESS OF BELOW ERROR HANDLING...
  # empty <- which(vapply(x, length, integer(1)) == 0)
  # if(length(empty)) x <- x[-empty]
  # not_df <- which(vapply(x, is.data.frame, logical(1)) == FALSE)
  # if(length(not_df))
  #   x <- c(list(category = tibble::as_tibble(x[not_df])), x[-not_df])
  # purrr::modify_if(x, is.data.frame, tibble::as_tibble)
}

.eia_dir_memoized <- memoise::memoise(.eia_dir)

.eia_dir_url <- function(dir, key){
  .eia_url(path = paste0(dir, "/?api_key=", key))
}
