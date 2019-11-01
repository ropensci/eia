globalVariables(".data")

#' eia: EIA API wrapper
#'
#' This package provides API access to data from the US \href{https://www.eia.gov/}{Energy Information Administration} (EIA).
#' @docType package
#' @name eia
NULL

#' @importFrom tibble tibble
NULL

#' Pipe operator
#'
#' See \code{magrittr} package for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

.eia_url <- function(key, id = NULL,
                     endpoint = c("category", "series", "series/categories",
                                  "geoset", "relation"),
                     relation = FALSE){
  endpoint <- match.arg(endpoint)
  if(is.null(id)){
    id <- "?"
  } else {
    if(endpoint == "relation"){
      epid <- "geoset"
    } else if(endpoint == "series/categories"){
      epid <- "series"
    } else {
      epid <- endpoint
    }
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

#' @importFrom httr GET content
.eia_get <- function(x){
  .antidos_before("eia")
  x <- httr::RETRY(
    verb = "GET"
    , url = x
    , .session_eia_env$ua
  )
  .antidos_after("eia")
  if(x$status_code == "404") stop("Page not found", call. = FALSE)
  httr::content(x, as = "text", encoding = "UTF-8")
}

.antidos_before <- function(x, sec = getOption("eia_antidos", 1)){
  wait <- 0
  if(!is.null(.session_eia_env[[x]])){
    wait <- as.numeric(get(x, .session_eia_env)) + sec - as.numeric(Sys.time())
    if(wait > 0) Sys.sleep(wait) else wait <- 0
  }
  assign(x, Sys.time(), envir = .session_eia_env)
  wait
}

.antidos_after <- function(x){
  assign(x, Sys.time(), envir = .session_eia_env)
}

.key_check <- function(key){
  if(is.null(key)) stop("Key is missing.", call. = FALSE)
}
