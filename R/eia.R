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

.eia_url <- function(api_key, id = NULL, endpoint = c("category", "series", "geoset")){
  endpoint <- match.arg(endpoint)
  if(is.null(id)){
    id <- "?"
  } else {
    id <- paste0("?", endpoint, "_id=", paste0(id, collapse = ";"), "&")
  }
  paste0("http://api.eia.gov/", endpoint, "/", id, "api_key=", api_key, "&out=json")
}

.eia_time_params <- function(start = NULL, end = NULL, n = NULL, n_default = 1){
  n_warn <- "Cannot requestion more than 100 results."
  if(!is.null(start)) n <- NULL
  if(!is.null(n) && n > 100){
    warning(n_warn, call. = FALSE)
    n <- 100
  }
  if(is.null(start) & is.null(end)){
    if(is.null(n)) n <- n_default
    return(list(start = start, end = end, n = n))
  }
  if(!is.null(start) & !is.null(end)){
    if(end - start + 1 > 100) warning(n_warn, call. = FALSE)
    return(list(start = start, end = end, n = NULL))
  }
  if(is.null(n) & is.null(start)) n <- n_default
  list(start = start, end = end, n = n)
}
