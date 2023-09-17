#' EIA data series
#'
#' Obtain EIA data series.
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
#' @param id character, series ID, may be a vector.
#' @param start start date. Providing only a start date will return up to the maximum 100 results if available.
#' @param end end date. Providing only an end date will a single result for that date.
#' @param n integer, length of series to return ending at most recent value or at \code{end} date if also provided. Ignored if \code{start} is not \code{NULL}.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization. See details.
#' @param key API key: character if set explicitly; not needed if key is set globally. See \code{\link{eia_set_key}}.
#'
#' @return a tibble data frame (or a list, or character, depending on \code{tidy} value)
#' @export
#' @seealso \code{\link{eia_clear_cache}}
#'
#' @examples
#' \dontrun{
#' # use eia_set_key() to store stored API key
#' id <- paste0("ELEC.GEN.ALL-AK-99.", c("A", "Q", "M"))
#'
#' x1 <- eia_series(id[1], start = 2016)
#' x2 <- eia_series(id[2], n = 5)
#' x3 <- eia_series(id[3], end = 2016, n = 5)
#' x1$data[[1]]
#' x2$data[[1]]
#' x3$data[[1]]
#'
#' # multiple series counted as a single API call
#' x <- eia_series(id, end = 2016, n = 2)
#' x$data
#' }
eia_series <- function(id, start = NULL, end = NULL, n = NULL,
                       tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  if(cache) .eia_series_memoized(id, start, end, n, tidy, key) else
    .eia_series(id, start, end, n, tidy, key)
}

.eia_series <- function(id, start = NULL, end = NULL,
                        n = NULL, tidy = TRUE, key){
  x <- .eia_series_url(id, start, end, n, key) %>% .eia_get()
  if(is.na(tidy)) return(x)
  x <- jsonlite::fromJSON(x)
  if("error" %in% names(x$data))
    stop(paste("API error:", x$data$error[1]), call. = FALSE)
  if(!tidy) return(x)
  x <- x$series
  idx <- vapply(x$data, function(i) length(i) > 0, logical(1))
  if(any(idx)) x <- x[idx, ]
  f <- function(i){
    if(!length(x$data[[i]])){
      warning(paste0("No data returned for id: ", id[i], "."), call. = FALSE)
      return()
    }
    x <- as.data.frame(x$data[[i]], stringsAsFactors = FALSE) %>%
      stats::setNames(c("date", "value")) %>%
      tibble::as_tibble() %>%
      .parse_series_eiadate(x$f[i])
    suppressWarnings(x$value <- as.numeric(x$value))
    x
  }
  x$data <- lapply(seq_len(nrow(x)), f)
  tibble::as_tibble(x)
}

.eia_series_memoized <- memoise::memoise(.eia_series)

.parse_series_eiadate <- function(d, date_format){
  d <- dplyr::rename(d, date0 = "date")
  d$date <- eiadate_to_date(d$date0)
  if(date_format == "A"){
    d$year <- as.integer(d$date0)
  } else if(date_format == "Q"){
    x <- strsplit(d$date0, "Q")
    d$year <- as.integer(sapply(x, "[", 1))
    d$qtr <- as.integer(sapply(x, "[", 2))
  } else {
    d$year <- as.integer(substr(d$date0, 1, 4))
    d$month <- as.integer(substr(d$date0, 5, 6))
    if(date_format != "M"){
      d$week <- as.integer(lubridate::isoweek(d$date))
    }
  }
  d$date0 <- NULL
  d
}

.eia_series_url <- function(id, start, end, n, key){
  params <- .eia_time_params(start, end, n)
  url <- .eia_url(key, id, "series")
  if(!is.null(params$start)) url <- paste0(url, "&start=", params$start)
  if(!is.null(params$end)) url <- paste0(url, "&end=", params$end)
  if(!is.null(params$n)) url <- paste0(url, "&num=", params$n)
  url
}

#' EIA series metadata
#'
#' Make a small request to obtain a data frame containing metadata.
#'
#' Dates are provided in \code{eia_series_dates} for the convenience of working with the EIA date string format;
#' for example: maintaining order, generating sequences, computing intervals,
#' and other operations that work well with dates but would be difficult using arbitrary strings.
#' Keep in mind that of course these are not real dates, in the sense that you cannot map a year to a specific date.
#'
#' \code{eia_series_updates} returns a data frame of most recent series update times for \code{id}.
#' Like the other metadata helpers, this does require an API call to the series to obtain the relevant metadata.
#' This can be useful if you are only interested in these update times for a specific set of series IDs.
#' If you need to know the most recent update stamps for a large set of series, you should use \code{\link{eia_updates}}
#' instead, which makes an API call specifically to the EIA \code{updates} endpoint for specific EIA categories by category ID.
#'
#' \code{eia_series_cats} differs from the other functions in that it makes an API call directly to the \code{series categories} endpoint.
#' Like other functions that return endpoint-specific output, it accepts the \code{tidy} argument for control over output structure.
#' By default, additional processing is done to return a list containing tibble data frames.
#' Set \code{tidy = FALSE} to return only the initial list result of \code{jsonlite::fromJSON}.
#' Set \code{tidy = NA} to return the original JSON as a character string.
#'
#' @param id character, series ID, may be a vector.
#' @param tidy logical, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization.
#' @param key API key: character if set explicitly; not needed if key is set globally. See \code{\link{eia_set_key}}.
#'
#' @return a tibble data frame
#' @export
#' @name eia_series_metadata
#' @seealso \code{\link{eia_updates}}
#'
#' @examples
#' \dontrun{
#' # use eia_set_key() to store stored API key
#' id <- paste0("ELEC.CONS_TOT_BTU.COW-AK-1.", c("A", "Q", "M"))
#'
#' eia_series_metadata(id)
#' eia_series_updates(id)
#' eia_series_dates(id)
#' eia_series_range(id)
#' eia_series_cats(id)
#' }
eia_series_metadata <- function(id, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  x <- if(cache) .eia_series_memoized(id, n = 1, key = key) else
    .eia_series(id, n = 1, key = key)
  dplyr::select(x, -c("data"))
}

#' @export
#' @name eia_series_metadata
eia_series_updates <- function(id, cache = TRUE, key = eia_get_key()){
  x <- eia_series_metadata(id, cache, key)
  dplyr::select(x, c("series_id", "updated"))
}

#' @export
#' @name eia_series_metadata
eia_series_dates <- function(id, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  x <- if(cache) .eia_series_memoized(id, n = 1, key = key) else
    .eia_series(id, n = 1, key = key)
  x <- split(x, seq_len(nrow(x)))
  f <- function(x){
    date_format <- eiadate_format(x$start)
    dates <- eiadate_to_date_seq(x$start, x$end)
    tibble::tibble(
      series_id = x$series_id,
      date = dates,
      eiadate = date_to_eiadate(dates, date_format),
      date_format = date_format
    )
  }
  purrr::map_dfr(x, f)
}

#' @export
#' @name eia_series_metadata
eia_series_range <- function(id, cache = TRUE, key = eia_get_key()){
  x <- eia_series_dates(id, cache, key)
  x <- split(x, factor(x$series_id, levels = unique(x$series_id)))
  f <- function(x){
    n <- nrow(x)
    tibble::tibble(
      series_id = x$series_id[1],
      start_date = min(x$date),
      end_date = max(x$date),
      start = x$eiadate[1],
      end = x$eiadate[n],
      date_format = x$date_format[1],
      n = n[1]
    )
  }
  purrr::map_dfr(x, f)
}

#' @export
#' @name eia_series_metadata
eia_series_cats <- function(id, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  if(cache) .eia_series_cats_memoized(id, tidy, key) else
    .eia_series_cats(id, tidy, key)
}

.eia_series_cats <- function(id, tidy = TRUE, key){
  x <- .eia_series_cats_url(id, key) %>% .eia_get()
  if(is.na(tidy)) return(x)
  x <- jsonlite::fromJSON(x)
  if("error" %in% names(x$data))
    stop(paste("API error:", x$data$error[1]), call. = FALSE)
  if(!tidy) return(x)
  x <- x$series_categories
  idx <- vapply(x$categories, function(i) length(i) > 0, logical(1))
  if(any(idx)) x <- x[idx, ]
  f <- function(i){
    x <- as.data.frame(x$categories[[i]], stringsAsFactors = FALSE) %>%
      stats::setNames(c("category_id", "name")) %>%
      tibble::as_tibble()
    x$category_id <- as.integer(x$category_id)
    dplyr::mutate(x, series_id = id[i]) %>% dplyr::select(c(3, 1, 2))
  }
  purrr::map_dfr(seq_len(nrow(x)), f)
}

.eia_series_cats_memoized <- memoise::memoise(.eia_series_cats)

.eia_series_cats_url <- function(id, key){
  .eia_url(key, id, "series/categories")
}
