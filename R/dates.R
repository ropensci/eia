#' EIA date parsing
#'
#' Helper functions for manipulating and converting between regular year-month-day date strings and EIA date string notation.
#'
#' There is no reason to mix EIA date formats in this context. Functions that take EIA date strings expect a consistent format.
#' Also, EIA date formats are parsed automatically from the dates themselves.
#' However, daily and weekly use the same format. Too avoid ambiguity in `eia_date_seq`, daily is assumed; set `weekly = TRUE` to treat as weekly.
#'
#' When providing a real date or date string, such as to `date_to_eiadate`, dates should be in `YYYY-MM-DD` format,
#' or at least any format that can be parsed by `lubridate::ymd` or `lubridate::ymd_hms` for dates and hourly date times, respectively.
#'
#' `"LH"` is not a supported date format. Use `"H"`. The API does not
#' translate the date and time when using `"LH"` anyhow; it simply appends
#' the date string with the number of hours time difference.
#'
#' @param x character, EIA date string; character or date object for regular dates. See details.
#' @param start start EIA date or date.
#' @param end end EIA date or date.
#' @param date_format EIA date format: "A", "Q", "M", "W", "D", "H".
#' These stand for annual, quarterly, monthly, weekly, daily, hourly. See details.
#' @param weekly logical. See details.
#'
#' @export
#' @name eiadate
#'
#' @examples
#' eiadate_to_date(c("201803", "201804"))
#'
#' date_to_eiadate("2018-05-14", "A")
#' date_to_eiadate("2018-05-14", "Q")
#' date_to_eiadate("2018-05-14", "M")
#'
#' (x <- eiadate_to_date_seq("2018-Q1", "2018-Q4"))
#' date_to_eiadate(x, "Q")
#' date_to_eiadate(x, "M")
#'
#' (x <- eiadate_to_date("2019-01-02T16Z"))
#' date_to_eiadate(x, "H")
#' (x <- eiadate_to_date_seq("2019-01-02T16Z", "2019-01-02T19Z"))
#' date_to_eiadate(x, "H")
eiadate_to_date <- function(x){
  date_format <- eiadate_format(x)
  .check_hl(date_format)
  switch(
    date_format,
    "A" = lubridate::ymd(x, truncated = 2L),
    "Q" = lubridate::yq(x),
    "M" = lubridate::ymd(x, truncated = 1L),
    "D" = lubridate::ymd(x),
    "H" = lubridate::ymd_h(x)
  )
}

#' @export
#' @rdname eiadate
date_to_eiadate <- function(x, date_format = c("A", "Q", "M", "W", "D", "H")){
  .check_hl(date_format[1])
  date_format <- match.arg(date_format)
  x <- if(date_format == "H") lubridate::ymd_hms(x) else lubridate::ymd(x)
  if(date_format == "A"){
    substr(as.character(x), 1, 4)
  } else if(date_format == "Q"){
    gsub("\\.", "-Q", lubridate::quarter(x, with_year = TRUE))
  } else if(date_format == "M"){
    gsub("-", "-", substr(as.character(x), 1, 7))
  } else if(date_format == "H"){
    paste0(gsub(" ", "T", substr(as.character(x), 1, 13)), "Z")
  } else if(date_format %in% c("W", "D")){
    as.character(x)
  }
}

.check_hl <- function(x){
  if(x == "LH")
    stop("`LH` date format is not supported. Use `H`.", call. = FALSE)
}

#' @export
#' @rdname eiadate
eiadate_to_date_seq <- function(start, end, weekly = FALSE){
  date_format <- eiadate_format(start, weekly)
  x <- eiadate_to_date(c(start, end))
  i <- switch(
    date_format,
    "A" = "year",
    "Q" = "quarter",
    "M" = "month",
    "W" = "week",
    "D" = "day",
    "H" = "hour",
    "LH" = "hour"
  )
  seq(x[1], x[2], by = i)
}

is_eiadate <- function(x){
  if(!is.character(x)) return(FALSE)
  grepl("^\\d\\d\\d\\d((0[1-9]|1[0-2])|)$", x) |
    grepl("^\\d\\d\\d\\d-Q(1|2|3|4)$", x) |
    grepl("^\\d\\d\\d\\d-\\d\\d$", x) |
    grepl("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", x) |
    grepl("^\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d(Z|-\\d\\d)$", x)
}

eiadate_format <- function(x, weekly = FALSE){
  if(any(!is_eiadate(x))) stop("Not an EIA format date string.", call. = FALSE)
  if(grepl("^\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\dZ$", x[1])){
    "H"
  } else if(grepl("^\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d\\d\\d$", x[1])){
    "LH"
  } else if(grepl("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", x[1])){
    if(weekly) "W" else "D"
  } else if(grepl("^\\d\\d\\d\\d-\\d\\d$", x[1])){
    "M"
  } else if(grepl("^\\d\\d\\d\\d$", x[1])){
    "A"
  } else if(grepl("Q", x[1])){
    "Q"
  }
}
