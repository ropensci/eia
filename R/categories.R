#' EIA categories
#'
#' Obtain EIA categories.
#'
#' Set \code{tidy = FALSE} to return only the initial result of \code{jsonlite::fromJSON}.
#' By default, additional processing is done to condense some list items into data frames and all data frames are converted to tibble data frames.
#'
#' \code{eia_child_cats} returns only the immediate child categories. \code{eia_parent_cats} returns all parents.
#'
#' @param api_key character value, API key.
#' @param id integer, category id.
#' @param tidy logical, return a tidier result. See details.
#'
#' @return a list for \code{eia_cats}; others functions return a tibble data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' key <- readRDS("key.rds") # your stored API key
#' eia_cats(key)
#'
#' eia_child_cats(key, 389) # immedate children
#' eia_parent_cats(key, 742) # all parents
#' }
eia_cats <- function(api_key, id = NULL, tidy = TRUE){
  x <- .eia_cat_url(api_key, id) %>% httr::GET() %>%
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

#' @export
#' @rdname eia_cats
eia_child_cats <- function(api_key, id){
  if(!is.numeric(id)) stop("`id` must be a number.", call. = FALSE)
  eia_cats(api_key, id)$childcategories
}

#' @export
#' @rdname eia_cats
eia_parent_cats <- function(api_key, id){
  if(!is.numeric(id)) stop("`id` must be a number.", call. = FALSE)
  f <- function(api_key, id, d = NULL){
    x <- eia_cats(api_key, id)$category
    done <- !"parent_category_id" %in% names(x)
    d <- dplyr::bind_rows(x, d)
    if(done) d else Recall(api_key, x$parent_category_id, d)
  }
  f(api_key, id)
}

.eia_cat_url <- function(api_key, id = NULL){
  id <- if(is.null(id)) "?" else paste0("?category_id=", id, "&")
  paste0("http://api.eia.gov/category/", id, "api_key=", api_key, "&out=json")
}
