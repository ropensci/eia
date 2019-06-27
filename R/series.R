#' EIA data series
#'
#' Obtain EIA data series.
#'
#' Set \code{tidy = FALSE} to return only the initial result of \code{jsonlite::fromJSON}.
#' By default, additional processing is done to return a tibble data frame.
#'
#' @param api_key character, API key.
#' @param id character, series ID, may be a vector.
#' @param start start date. Providing only a start date will return up to the maximum 100 results if available.
#' @param end end date. Providing only an end date will a single result for that date.
#' @param n integer, length of series to return ending at most recent value or at \code{end} date if also provided. Ignored if \code{start} is not \code{NULL}.
#' @param tidy logical, return a tidier result. See details.
#'
#' @return a tibble data frame
#' @export
#'
#' @examples
#' \dontrun{
#' key <- readRDS("key.rds") # your stored API key
#' id <- paste0("ELEC.CONS_TOT_BTU.COW-AK-1.", c("A", "Q", "M"))
#'
#' x1 <- eia_series(key, id[1], start = 2016)
#' x2 <- eia_series(key, id[2], n = 10)
#' x3 <- eia_series(key, id[3], end = 2016, n = 10)
#' x1$data[[1]]
#' x2$data[[1]]
#' x3$data[[1]]
#'
#' # multiple series counted as a single API call
#' x <- eia_series(key, id, end = 2016, n = 10)
#' x$data[[1]]
#' }
eia_series <- function(api_key, id, start = NULL, end = NULL, n = NULL, tidy = TRUE){
  x <- .eia_series_url(api_key, id, start, end, n) %>% httr::GET() %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  if(!tidy) return(x)

  x <- x$series
  x <- x[sapply(x$data, function(i) length(i) > 0), ]
  if(is.null(x) || nrow(x) == 0) return()

  f <- function(name) c("date", "value")
  f2 <- function(i){
    x <- tibble::as_tibble(x$data[[i]], .name_repair = f) %>%
      .eia_date(x$f[i])
    x$value <- as.numeric(x$value)
    dplyr::select(x, c(2:ncol(x), 1))
  }
  x$data <- lapply(1:nrow(x), f2)
  tibble::as_tibble(x)
}

.eia_date <- function(d, date_format){
  if(date_format == "Q"){
    x <- strsplit(d$date, "[Qq]")
    d$year <- as.integer(sapply(x, "[", 1))
    d$qtr <- as.integer(sapply(x, "[", 2))
  } else if(date_format == "M"){
    d$year <- as.integer(substr(d$date, 1, 4))
    d$month <- as.integer(substr(d$date, 5, 6))
  } else if(date_format == "A"){
    d$year <- as.integer(d$date)
  } else {
    stop("Unknown date format.", call. = FALSE)
  }
  d$date <- NULL
  d
}

.eia_series_url <- function(api_key, id, start = NULL, end = NULL, n = NULL){
  params <- .eia_time_params(start, end, n, n_default = 1)
  url <- .eia_url(api_key, id, "series")
  if(!is.null(params$start)) url <- paste0(url, "&start=", params$start)
  if(!is.null(params$end)) url <- paste0(url, "&end=", params$end)
  if(!is.null(params$n)) url <- paste0(url, "&num=", params$n)
  url
}
