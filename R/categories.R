#' EIA categories
#'
#' Obtain EIA categories.
#'
#' By default, additional processing is done to return a list containing tibble data frames.
#' Set \code{tidy = FALSE} to return only the initial list result of \code{jsonlite::fromJSON}.
#' Set \code{tidy = NA} to return the original JSON as a character string.
#'
#' Set to \code{cache = FALSE} to force a new API call for updated data.
#' Using \code{FALSE} always makes a new API call and returns the result from the server.
#' \code{TRUE} uses memoization on a per R session basis, caching the result of the function call in memory for the duration of the R session.
#' You can reset the entire cache by calling \code{eia_clear_cache}.
#'
#' \code{eia_child_cats} returns only the immediate child categories. \code{eia_parent_cats} returns all parents.
#' These are wrappers around \code{eia_cats} and always return a tibble data frame.
#'
#' @param id integer, category ID. If \code{NULL}, the API root category.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization. See details.
#' @param key API key: character if set explicitly; not needed if key is set globally. See \code{\link{eia_set_key}}.
#'
#' @return for \code{eia_cats}, a list of tibble data frames (or a less processed list, or character, depending on \code{tidy} value); others functions return a tibble data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' # use eia_set_key() to store stored API key
#' eia_cats()
#'
#' eia_child_cats(389) # immedate children
#' eia_parent_cats(742) # all parents
#' }
eia_cats <- function(id = NULL, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  if(cache) .eia_cats_memoized(id, tidy, key) else
    .eia_cats(id, tidy, key)
}

.eia_cats <- function(id, tidy, key){
  x <- .eia_cat_url(id, key) %>% .eia_get()
  if(is.na(tidy)) return(x)
  x <- jsonlite::fromJSON(x)
  if(!tidy) return(x)

  x <- x$category
  empty <- which(vapply(x, length, integer(1)) == 0)
  if(length(empty)) x <- x[-empty]
  not_df <- which(vapply(x, is.data.frame, logical(1)) == FALSE)
  if(length(not_df))
    x <- c(list(category = tibble::as_tibble(x[not_df])), x[-not_df])
  purrr::modify_if(x, is.data.frame, tibble::as_tibble)
}

.eia_cats_memoized <- memoise::memoise(.eia_cats)

#' @export
#' @rdname eia_cats
eia_child_cats <- function(id, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  eia_cats(id, cache = cache, key = key)$childcategories
}

#' @export
#' @rdname eia_cats
eia_parent_cats <- function(id, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  f <- function(key, id, d = NULL){
    x <- eia_cats(id, cache = cache, key = key)$category
    done <- !"parent_category_id" %in% names(x)
    d <- dplyr::bind_rows(x, d)
    if(done) d else Recall(key, x$parent_category_id, d)
  }
  f(key, id)
}

.eia_cat_url <- function(id, key) .eia_url(key, id, "category")

#' EIA data updates
#'
#' Obtain information on EIA data series updates for a given category to avoid having to make requests for data that have not been updated since your last request.
#'
#' This function returns paginated results of the most recent update dates for data series.
#' \code{n} and \code{start} help with stepping through chunks.
#'
#' If you need to know the most recent update stamps for a large set of series, you should use this function,
#' which makes an API call specifically to the EIA \code{updates} endpoint for specific EIA categories by category ID.
#' If you are only interested in update times for a specific set of series IDs,
#' you can use \code{\link{eia_series_updates}}.
#' Note that while this function accepts a vector of IDs for \code{id}, it must make one API call per ID.
#'
#' By default, additional processing is done to return a tibble data frame.
#' Set \code{tidy = FALSE} to return only the initial list result of \code{jsonlite::fromJSON}.
#' Set \code{tidy = NA} to return the original JSON as a character string.
#'
#' @param id integer, category ID, may be a vector. If \code{NULL}, the API root category.
#' @param deep logical, if \code{TRUE}, return information on all child series. If \code{FALSE} (default), return only for the category \code{id}.
#' @param n integer, maximum number of rows of series to return. Defaults to 50; maximum permitted by the API is 10,000.
#' @param start integer, row to start from, defaults to 1.
#' @param tidy logical, return a tidier result. See details.
#' @param key API key: character if set explicitly; not needed if key is set globally. See \code{\link{eia_set_key}}.
#'
#' @return a tibble data frame (or a list, or character, depending on \code{tidy} value)
#' @export
#' @seealso \code{\link{eia_series_updates}}
#'
#' @examples
#' \dontrun{
#' # use eia_set_key() to store stored API key
#' eia_updates(742, n = 5)
#' }
eia_updates <- function(id = NULL, deep = FALSE, n = 50, start = 1,
                        tidy = TRUE, key = eia_get_key()){
  .key_check(key)
  f <- if(is.na(tidy) || !tidy) purrr::map else purrr::map_dfr
  x <- f(if(is.null(id)) -1 else id,
         ~.eia_updates(.x, deep, n, start, tidy, key))
  if(!is.data.frame(x)){
    if(is.character(x[[1]])) x <- unlist(x)
  } else if(nrow(x) == 0){
    x <- tibble(series_id = character(), updated = character())
  } else if(length(id) > 1){
    x <- dplyr::distinct_at(x, c("series_id", "updated"))
  }
  x
}

.eia_updates <- function(id, deep, n, start, tidy, key){
  if(id == -1){
    id <- "?"
  } else {
    id <- paste0("?category_id=", id, "&")
  }
  url <- paste0("https://api.eia.gov/updates/", id, "api_key=", key,
                "&deep=", tolower(as.character(deep)),
                "&rows=", n, "&firstrow=", start - 1, "&out=json")
  x <- .eia_get(url)
  if(is.na(tidy)) return(x)
  x <- jsonlite::fromJSON(x)
  if(!tidy) return(x)
  x <- x$updates
  if(is.data.frame(x)) tibble::as_tibble(x) else tibble::tibble()
}
