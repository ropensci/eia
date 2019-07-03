#' EIA date parsing
#'
#' Helper functions for manipulating and converting between regular year-month-day date strings and EIA date string notation.
#'
#' There is no reason to mix EIA date formats in this context. Functions that take EIA date strings expect a consistent format.
#'
#' @param x character, EIA date string; character or date object for for regular dates. Dates should be in year-month-day order.
#' @param start start EIA date or date.
#' @param end end EIA date or date.
#' @param date_format EIA date format: "A", "Q", "M". These stand for annual, quarterly, monthly.
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
#' date_to_eiadate(x)
eiadate_to_date <- function(x){
  date_format <- eiadate_format(x)
  switch(date_format,
         "Q" = lubridate::yq(x),
         "M" = lubridate::ymd(x, truncated = 1L),
         "A" = lubridate::ymd(x, truncated = 2L))
}

#' @export
#' @rdname eiadate
date_to_eiadate <- function(x, date_format = c("A", "Q", "M")){
  date_format <- match.arg(date_format)
  if(date_format == "Q"){
    gsub("\\.", "Q", lubridate::quarter(x, with_year = TRUE))
  } else if(date_format == "M"){
    gsub("-", "", substr(as.character(x), 1, 7))
  } else if(date_format == "A"){
    substr(as.character(x), 1, 4)
  } else {
    stop("Unknown date format.", call. = FALSE)
  }
}

#' @export
#' @rdname eiadate
eiadate_to_date_seq <- function(start, end){
  date_format <- eiadate_format(start)
  x <- eiadate_to_date(c(start, end))
  i <- switch(date_format,
              "Q" = "quarter", "M" = "month", "A" = "year",
              "W" = "week", "D" = "day")
  seq(x[1], x[2], by = i)
}

is_eiadate <- function(x){
  if(!is.character(x)) return(FALSE)
  (grepl("^\\d\\d\\d\\d((0[1-9]|1[0-2])|)$", x)) |
    grepl("^\\d\\d\\d\\d(Q|q)(1|2|3|4)$", x)
}

eiadate_format <- function(x){
  if(any(!is_eiadate(x))) stop("Not an EIA format date string.", call. = FALSE)
  if(grepl("Q|q", x[1])){
    "Q"
  } else if(grepl("^\\d\\d\\d\\d$", x[1])) {
    "A"
  } else if(grepl("^\\d\\d\\d\\d\\d\\d$", x[1])){
    "M"
  } else {
    print("Unhandled case")
  }
}
