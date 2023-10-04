#' EIA categories
#'
#' Obtain EIA categories.
#'
#' By default, additional processing is done to return a list containing tibble data frames.
#' Set `tidy = FALSE` to return only the initial list result of `jsonlite::fromJSON`.
#' Set `tidy = NA` to return the original JSON as a character string.
#'
#' Set to `cache = FALSE` to force a new API call for updated data.
#' Using `FALSE` always makes a new API call and returns the result from the server.
#' `TRUE` uses memoization on a per R session basis, caching the result of the function call in memory for the duration of the R session.
#' You can reset the entire cache by calling `eia_clear_cache`.
#'
#' `eia_subcategories` returns only the immediate subcategories under a given parent.
#' This is a wrapper around `eia_categories` and always return a tibble data frame.
#'
#' @param cat character, category id, if `NULL` then the API root directory.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization. See details.
#' @param key API key: character if set explicitly; not needed if key is set globally. See `eia_set_key()`.
#'
#' @return for `eia_categories`, a tibble data frame (or a less processed list, or character, depending on `tidy` value); others functions return a tibble data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' # use eia_set_key() to store API key
#' eia_categories()
#'
#' eia_subcategories("electricity")
#' }
eia_categories <- function(cat = NULL, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  if(cache) .eia_cats_memoized(cat, tidy, key) else .eia_cats(cat, tidy, key)
}

#' @export
#' @rdname eia_categories
eia_subcategories <- function(cat, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  eia_categories(cat, cache = cache, key = key)
}

#' @export
#' @rdname eia_categories
eia_cats <- function(id = NULL, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .Deprecated("eia_categories")
  eia_categories(cat = id, tidy, cache, key)
}

#' @export
#' @rdname eia_categories
eia_parent_cats <- function(id, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .Deprecated("eia_categories")
  eia_categories(cat = id, tidy, cache, key)
}

#' @export
#' @rdname eia_categories
eia_child_cats <- function(id, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .Deprecated("eia_subcategories")
  eia_subcategories(cat = id, tidy, cache, key)
}

.eia_cats <- function(cat, tidy, key){
  x <- .eia_cat_url(cat, key) |> .eia_get()
  if(is.na(tidy)) return(x)
  x <- jsonlite::fromJSON(x)
  if(!tidy) return(x)
  x <- x$response$routes
  if(tidy && is.data.frame(x))
    tibble::as_tibble(sapply(x, function(x) { gsub("( \\r\\n) *", " ", x) }))

  ## NOT SURE OF USEFULNESS OF BELOW ERROR HANDLING...
  # empty <- which(vapply(x, length, integer(1)) == 0)
  # if(length(empty)) x <- x[-empty]
  # not_df <- which(vapply(x, is.data.frame, logical(1)) == FALSE)
  # if(length(not_df))
  #   x <- c(list(category = tibble::as_tibble(x[not_df])), x[-not_df])
  # purrr::modify_if(x, is.data.frame, tibble::as_tibble)
}

.eia_cats_memoized <- memoise::memoise(.eia_cats)

.eia_cat_url <- function(cat, key){
  .eia_url(path = paste0(cat, "/?api_key=", key))
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
