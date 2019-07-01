#' EIA categories
#'
#' Obtain EIA categories.
#'
#' Set \code{tidy = FALSE} to return only the initial result of \code{jsonlite::fromJSON}.
#' By default, additional processing is done to condense some list items into data frames and all data frames are converted to tibble data frames.
#'
#' Set to \code{cache = FALSE} to force a new API call for updated data.
#' Using \code{FALSE} always makes a new API call and returns the result from the server.
#' \code{TRUE} uses memoization on a per R session basis, caching the result of the function call in memory for the duration of the R session.
#' You can reset the entire cache by calling \code{eia_clear_cache}.
#'
#' \code{eia_child_cats} returns only the immediate child categories. \code{eia_parent_cats} returns all parents.
#'
#' @param key character, API key.
#' @param id integer, category ID.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization. See details.
#'
#' @return a list for \code{eia_cats}; others functions return a tibble data frame.
#' @export
#' @seealso \code{\link{eia_clear_cache}}
#'
#' @examples
#' \dontrun{
#' key <- Sys.getenv("EIA_KEY") # your stored API key
#' eia_cats(key)
#'
#' eia_child_cats(key, 389) # immedate children
#' eia_parent_cats(key, 742) # all parents
#' }
eia_cats <- function(key, id = NULL, tidy = TRUE, cache = TRUE){
  if(cache) .eia_cats_memoized(key, id, tidy) else
    .eia_cats(key, id, tidy)
}

.eia_cats <- function(key, id = NULL, tidy = TRUE){
  x <- .eia_cat_url(key, id) %>% .eia_get() %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  if(!tidy) return(x)

  x <- x$category
  empty <- which(sapply(x, length) == 0)
  if(length(empty)) x <- x[-empty]
  not_df <- which(sapply(x, is.data.frame) == FALSE)
  if(length(not_df))
    x <- c(list(category = tibble::as_tibble(x[not_df])), x[-not_df])
  purrr::modify_if(x, is.data.frame, tibble::as_tibble)
}

.eia_cats_memoized <- memoise::memoise(.eia_cats)

#' @export
#' @rdname eia_cats
eia_child_cats <- function(key, id, cache = TRUE){
  if(!is.numeric(id)) stop("`id` must be a number.", call. = FALSE)
  eia_cats(key, id, cache = cache)$childcategories
}

#' @export
#' @rdname eia_cats
eia_parent_cats <- function(key, id, cache = TRUE){
  if(!is.numeric(id)) stop("`id` must be a number.", call. = FALSE)
  f <- function(key, id, d = NULL){
    x <- eia_cats(key, id, cache = cache)$category
    done <- !"parent_category_id" %in% names(x)
    d <- dplyr::bind_rows(x, d)
    if(done) d else Recall(key, x$parent_category_id, d)
  }
  f(key, id)
}

.eia_cat_url <- function(key, id = NULL) .eia_url(key, id, "category")
