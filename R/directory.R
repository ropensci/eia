#' EIA directory
#'
#' Obtain EIA directory listing.
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
#' `eia_subdirectory` returns only the immediate sub-directories (folders/files) under a given parent.
#' This is a wrapper around `eia_directory` and always return a tibble data frame.
#'
#' @param dir character, directory path, if `NULL` then the API root directory.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization. See details.
#' @param key API key: character if set explicitly; not needed if key is set globally. See `eia_set_key()`.
#'
#' @return for `eia_directory`, a tibble data frame (or a less processed list, or character, depending on `tidy` value); others functions return a tibble data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' # use eia_set_key() to store API key
#' eia_directory()
#' eia_subdirectory("electricity")
#' eia_subdirectory("electricity/rto")
#' }
eia_directory <- function(dir = NULL, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  if(cache) .eia_dir_memoized(dir, tidy, key) else .eia_dir(dir, tidy, key)
}

#' @export
#' @rdname eia_directory
eia_subdirectory <- function(dir, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  eia_directory(dir, cache = cache, key = key)
}

#' @export
#' @rdname eia_directory
eia_dir <- function(id = NULL, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .Deprecated("eia_directory")
  eia_directory(dir = id, tidy, cache, key)
}

#' @export
#' @rdname eia_directory
eia_parent_cats <- function(id, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .Defunct("eia_categories")
}

#' @export
#' @rdname eia_directory
eia_child_cats <- function(id, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .Defunct("eia_subcategories")
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
    message(paste0(
      "No further sub-directories to discover.\n",
      'Use `eia::eia_metadata("', dir, '")` to explore this data.'
    ))
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


#' EIA data updates
#'
#' NOW DEFUNCT - FEATURE NO LONGER PROVIDED WITH APIv2.
#'
#' @return character sting, defunct message
#' @export
#'
#' @examples
#' eia_updates()
eia_updates <- function(...){
  .Defunct(msg = "eia::eia_updates() is defunct with no replacement - feature has been removed with APIv2.")
}
