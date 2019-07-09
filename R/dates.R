#' EIA date parsing
#'
#' Helper functions for manipulating and converting between regular year-month-day date strings and EIA date string notation.
#'
#' There is no reason to mix EIA date formats in this context. Functions that take EIA date strings expect a consistent format.
#' Also, EIA date formats are parsed automatically from the dates themselves.
#' However, daily and weekly use the same format. Too avoid ambiguity in \code{eia_date_seq}, daily is assumed; set \code{weekly = TRUE} to treat as weekly.
#'
#' When providing a real date or date string, such as to \code{date_to_eiadate}, dates should be in \code{yyyy-mm-dd} format,
#' or at least any format that can be parsed by \code{lubridate::ymd} or \code{lubridate::ymd_hms} for dates and hourly date times, respectively.
#'
#' @param x character, EIA date string; character or date object for regular dates. See details.
#' @param start start EIA date or date.
#' @param end end EIA date or date.
#' @param date_format EIA date format: "A", "Q", "M", "W", "D", "H". These stand for annual, quarterly, monthly, weekly, daily, hourly.
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
#' (x <- eiadate_to_date_seq("2018Q1", "2018Q4"))
#' date_to_eiadate(x, "Q")
#'
#' (x <- eiadate_to_date("20190102T16Z"))
#' date_to_eiadate(x, "H")
#' (x <- eiadate_to_date_seq("20190102T16Z", "20190102T19Z"))
#' date_to_eiadate(x, "H")
eiadate_to_date <- function(x){
  date_format <- eiadate_format(x)
  switch(date_format,
         "A" = lubridate::ymd(x, truncated = 2L),
         "Q" = lubridate::yq(x),
         "M" = lubridate::ymd(x, truncated = 1L),
         "D" = lubridate::ymd(x),
         "H" = lubridate::ymd_h(x))
}

#' @export
#' @rdname eiadate
date_to_eiadate <- function(x, date_format = c("A", "Q", "M", "W", "D", "H")){
  date_format <- match.arg(date_format)
  x <- if(date_format == "H") lubridate::ymd_hms(x) else lubridate::ymd(x)
  if(date_format == "A"){
    substr(as.character(x), 1, 4)
  } else if(date_format == "Q"){
    gsub("\\.", "Q", lubridate::quarter(x, with_year = TRUE))
  } else if(date_format == "M"){
    gsub("-", "", substr(as.character(x), 1, 7))
  } else if(date_format %in% c("W", "D")){
    gsub("-", "", as.character(x))
  } else if(date_format == "H"){
    gsub("(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d) (\\d\\d)", "\\1\\2\\3T\\4Z",
         substr(as.character(x), 1, 13))
  }
}

#' @export
#' @rdname eiadate
eiadate_to_date_seq <- function(start, end, weekly = FALSE){
  date_format <- eiadate_format(start, weekly)
  x <- eiadate_to_date(c(start, end))
  i <- switch(date_format,
              "Q" = "quarter", "M" = "month", "A" = "year",
              "W" = "week", "D" = "day", "H" = "hour")
  seq(x[1], x[2], by = i)
}

is_eiadate <- function(x){
  if(!is.character(x)) return(FALSE)
  grepl("^\\d\\d\\d\\d((0[1-9]|1[0-2])|)$", x) |
    grepl("^\\d\\d\\d\\dQ(1|2|3|4)$", x) |
    grepl("^\\d\\d\\d\\d\\d\\d\\d\\d$", x) |
    grepl("^\\d\\d\\d\\d\\d\\d\\d\\dT\\d\\dZ$", x)
}

eiadate_format <- function(x, weekly = FALSE){
  if(any(!is_eiadate(x))) stop("Not an EIA format date string.", call. = FALSE)
  if(grepl("^\\d\\d\\d\\d\\d\\d\\d\\dT\\d\\dZ$", x[1])){
    "H"
  } else if(grepl("^\\d\\d\\d\\d\\d\\d\\d\\d$", x[1])){
    if(weekly) "W" else "D"
  } else if(grepl("^\\d\\d\\d\\d\\d\\d$", x[1])){
    "M"
  } else if(grepl("^\\d\\d\\d\\d$", x[1])){
    "A"
  } else if(grepl("Q", x[1])){
    "Q"
  }
}
