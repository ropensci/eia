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
#' @param key character, API key.
#' @param id character, series ID, may be a vector.
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
#' id <- paste0("ELEC.GEN.ALL-AK-99.", c("A", "Q", "M"))
#'
#' x1 <- eia_series(key, id[1], start = 2016)
#' x2 <- eia_series(key, id[2], n = 5)
#' x3 <- eia_series(key, id[3], end = 2016, n = 5)
#' x1$data[[1]]
#' x2$data[[1]]
#' x3$data[[1]]
#'
#' # multiple series counted as a single API call
#' x <- eia_series(key, id, end = 2016, n = 5)
#' x$data[[1]]
#' }
eia_series <- function(key, id, start = NULL, end = NULL, n = NULL,
                       tidy = TRUE, cache = TRUE){
  if(cache) .eia_series_memoized(key, id, start, end, n, tidy) else
    .eia_series(key, id, start, end, n, tidy)
}

.eia_series <- function(key, id, start = NULL, end = NULL,
                        n = NULL, tidy = TRUE){
  x <- .eia_series_url(key, id, start, end, n) %>% httr::GET() %>%
    httr::content(as = "text", encoding = "UTF-8")
  if(is.na(tidy)) return(x)
  x <- jsonlite::fromJSON(x)
  if(!tidy) return(x)

  x <- x$series
  x <- x[sapply(x$data, function(i) length(i) > 0), ]
  if(is.null(x) || nrow(x) == 0) return()

  f <- function(name) c("date", "value")
  f2 <- function(i){
    x <- tibble::as_tibble(x$data[[i]], .name_repair = f) %>%
      .parse_series_eiadate(x$f[i])
    x$value <- as.numeric(x$value)
    x
  }
  x$data <- lapply(1:nrow(x), f2)
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
  } else if(date_format %in% c("M", "W", "D")){
    d$year <- as.integer(substr(d$date0, 1, 4))
    d$month <- as.integer(substr(d$date0, 5, 6))
  } else if(date_format %in% c("W", "D")){
    d$week <- as.integer(lubridate::isoweek(d$date))
  } else {
    stop("Unknown date format.", call. = FALSE)
  }
  d$date0 <- NULL
  d
}

.eia_series_url <- function(key, id, start = NULL, end = NULL, n = NULL){
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
#' @param key character, API key.
#' @param id character, series ID, may be a vector.
#' @param cache logical, cache result for duration of R session using memoization.
#'
#' @return a tibble data frame
#' @export
#' @name eia_series_metadata
#'
#' @examples
#' \dontrun{
#' key <- Sys.getenv("EIA_KEY") # your stored API key
#' id <- paste0("ELEC.CONS_TOT_BTU.COW-AK-1.", c("A", "Q", "M"))
#'
#' eia_series_metadata(key, id)
#' eia_series_updated(key, id)
#' eia_series_dates(key, id)
#' eia_series_range(key, id)
#' }
eia_series_metadata <- function(key, id, cache = TRUE){
  x <- if(cache) .eia_series_memoized(key, id, n = 1) else
    .eia_series(key, id, n = 1)
  dplyr::select(x, -c("data"))
}

#' @export
#' @name eia_series_metadata
eia_series_updated <- function(key, id, cache = TRUE){
  x <- eia_series_metadata(key, id, cache)
  dplyr::select(x, c("series_id", "updated"))
}

#' @export
#' @name eia_series_metadata
eia_series_dates <- function(key, id, cache = TRUE){
  x <- if(cache) .eia_series_memoized(key, id, n = 1) else
    .eia_series(key, id, n = 1)
  x <- split(x, 1:nrow(x))
  f <- function(x){
    date_format <- eiadate_format(x$start)
    dates <- eiadate_to_date_seq(x$start, x$end)
    tibble::tibble(series_id = x$series_id, date = dates,
                   eiadate = date_to_eiadate(dates, date_format),
                   date_format = date_format)
  }
  purrr::map_dfr(x, f)
}

#' @export
#' @name eia_series_metadata
eia_series_range <- function(key, id, cache = TRUE){
  x <- eia_series_dates(key, id, cache)
  x <- split(x, factor(x$series_id, levels = unique(x$series_id)))
  f <- function(x){
    n <- nrow(x)
    tibble::tibble(series_id = x$series_id[1], start_date = min(x$date),
                   end_date = max(x$date),
                   start = x$eiadate[1], end = x$eiadate[n],
                   date_format = x$date_format[1], n = n[1])
  }
  purrr::map_dfr(x, f)
}
