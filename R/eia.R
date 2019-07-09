#' eia: EIA API
#'
#' This package provides API access to data from the US \href{https://www.eia.gov/}{Energy Information Administration} (EIA).
#' @docType package
#' @name eia
NULL

## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

.eia_url <- function(key, id = NULL,
                     endpoint = c("category", "series", "geoset", "relation"),
                     relation = FALSE){
  endpoint <- match.arg(endpoint)
  if(is.null(id)){
    id <- "?"
  } else {
    epid <- if(endpoint == "relation") "geoset" else endpoint
    id <- paste0("?", epid, "_id=", paste0(id, collapse = ";"), "&")
  }
  paste0("http://api.eia.gov/", endpoint, "/", id, "api_key=", key, "&out=json")
}

.eia_time_params <- function(start = NULL, end = NULL, n = NULL){
  if(!is.null(start)) n <- NULL
  if(is.null(start) & is.null(end)){
    return(list(start = start, end = end, n = n))
  }
  if(!is.null(start) & !is.null(end)){
    return(list(start = start, end = end, n = NULL))
  }
  list(start = start, end = end, n = n)
}

.eia_get <- function(x){
  .antidos_before("eia")
  x <- httr::GET(x)
  .antidos_after("eia")
  if(x$status_code == "404") stop("Page not found", call. = FALSE)
  httr::content(x, as = "text", encoding = "UTF-8")
}

.antidos_before <- function(x, sec = getOption("eia_antidos", 1)){
  wait <- 0
  if(!is.null(eia_api_time[[x]])){
    wait <- as.numeric(get(x, eia_api_time)) + sec - as.numeric(Sys.time())
    if(wait > 0) Sys.sleep(wait) else wait <- 0
  }
  assign(x, Sys.time(), envir = eia_api_time)
  wait
}

.antidos_after <- function(x){
  assign(x, Sys.time(), envir = eia_api_time)
}
