#' EIA geoset data
#'
#' Obtain EIA geoset data.
#'
#' \code{id} may be a vector. This should only be done with \code{tidy = TRUE} if the tidied results can be properly row bound.
#' The geoset API calls allow multiple regions, but expect a single series ID. This function will one API call per series ID.
#' There is an expectation of similarly formatted series that can be row bound. If the IDs are for differently structured data that cannot be tidily row bound,
#' you may as well make separate requests since each requires a unique API call either way.
#'
#' By default, additional processing is done to return a tibble data frame.
#' Set \code{tidy = FALSE} to return only the initial list result of \code{jsonlite::fromJSON}.
#' Set \code{tidy = NA} to return the original JSON as a character string.
#'
#' Set to \code{cache = FALSE} to force a new API call for updated data.
#' Using \code{FALSE} always makes a new API call and returns the result from the server.
#' \code{TRUE} uses memoization on a per R session basis, caching the result of the function call in memory for the duration of the R session.
#' You can reset the entire cache by calling \code{eia_clear_cache()}.
#'
#' @param key character, API key.
#' @param id character, geoset series ID, may be a vector. See details.
#' @param region character, region ID, may be a vector. Data available for the intersection of \code{id} and \code{region} is returned.
#' @param start start date. Providing only a start date will return up to the maximum 100 results if available.
#' @param end end date. Providing only an end date will a single result for that date.
#' @param n integer, length of series to return ending at most recent value or at \code{end} date if also provided. Ignored if \code{start} is not \code{NULL}.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization. See details.
#'
#' @return a tibble data frame (or a list, or character, depending on \code{tidy} value)
#' @export
#' @seealso \code{\link{eia_clear_cache}}
#'
#' @examples
#' \dontrun{
#' key <- Sys.getenv("EIA_KEY") # your stored API key
#' id <- paste0("ELEC.GEN.ALL-99.", c("A", "Q", "M"))
#' region <- c("USA-CA", "USA-NY")
#'
#' eia_geoset(key, id[1], region[1], start = 2016)
#' eia_geoset(key, id[2], region, n = 5)
#' eia_geoset(key, id[3], region[2], end = 2016, n = 5)
#'
#' # multiple series counted as a single API call
#' x <- eia_geoset(key, id, region[1], end = 2016, n = 5)
#' x$data[[1]]
#'
#' # Use direct US state abbreviations or names;
#' # Use US Census region and division names.
#' x <- eia_geoset(key, id[2], c("AK", "New England"), end = 2016, n = 1)
#' x$data[[1]]
#' }
eia_geoset <- function(key, id, region, start = NULL, end = NULL, n = NULL,
                       tidy = TRUE, cache = TRUE){
  region <- .to_state_abb(region)
  if(cache) .eia_geoset_memoized(key, id, region, start, end, n, tidy) else
    .eia_geoset(key, id, region, start, end, n, tidy)
}

.eia_geoset <- function(key, id, region, start = NULL, end = NULL,
                        n = NULL, tidy = TRUE){
  f <- if(is.na(tidy) || !tidy) purrr::map else purrr::map_dfr
  x <- f(id, ~.eia_geoset_by_id(key, .x, region, start, end, n, tidy))
  if(!is.data.frame(x)){
    if(is.character(x[[1]])) x <- unlist(x)
  }
  x
}

.eia_geoset_memoized <- memoise::memoise(.eia_geoset)

.eia_geoset_by_id <- function(key, id, region, start = NULL,
                              end = NULL, n = NULL, tidy = TRUE){
  x <- .eia_geo_url(key, id, region, start, end, n) %>% httr::GET() %>%
    httr::content(as = "text", encoding = "UTF-8")
  if(is.na(tidy)) return(x)
  x <- jsonlite::fromJSON(x)
  if(!tidy) return(x)

  x <- x$geoset
  empty <- which(sapply(x, length) == 0)
  if(length(empty)) x <- x[-empty]
  idx <- which(names(x) != "series")
  x <- list(geoset = tibble::as_tibble(x[idx]), series = x$series)

  f <- function(name) c("date", "value")
  f2 <- function(s){
    if(is.null(s$unitsshort)) s <- s[-which(names(s) == "unitsshort")]
    if(is.null(s$latlon)) s$latlon <- NA_character_
    s$data <- list(tibble::as_tibble(s$data, .name_repair = f) %>%
                     .parse_series_eiadate(x$geoset$f))
    s$data[[1]]$value <- as.numeric(s$data[[1]]$value)
    idx <- which(names(s) != "data")
    d <- tibble::as_tibble(s[idx])
    d$data <- s$data
    d
  }
  x$series <- purrr::map_dfr(x$series, f2)
  x$geoset <- dplyr::slice(x$geoset, rep(1, nrow(x$series)))
  dplyr::bind_cols(x$geoset, x$series)
}

.eia_geo_url <- function(key, id, region, start = NULL, end = NULL, n = NULL){
  params <- .eia_time_params(start, end, n, n_default = 1)
  url <- .eia_url(key, id, "geoset")
  url <- paste0(url, "&regions=", paste0(region, collapse = ";"))
  if(!is.null(params$start)) url <- paste0(url, "&start=", params$start)
  if(!is.null(params$end)) url <- paste0(url, "&end=", params$end)
  if(!is.null(params$n)) url <- paste0(url, "&num=", params$n)
  url
}

.to_state_abb <- function(x){
  f <- function(x){
    if(x %in% datasets::state.abb) return(paste0("USA-", x))
    y <- unique(c(
      datasets::state.abb[x == datasets::state.name],
      datasets::state.abb[x == datasets::state.division],
      datasets::state.abb[x == datasets::state.region]))
    if(length(y)) paste0("USA-", y) else x
  }
  unique(unlist(lapply(x, f)))
}
