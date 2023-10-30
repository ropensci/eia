globalVariables(".data")

#' eia: EIA API wrapper
#'
#' This package provides API access to data from the US
#' \href{https://www.eia.gov/}{Energy Information Administration} (EIA).
#' @docType package
#' @name eia
#' @aliases eia-package
NULL

#' @importFrom tibble tibble
NULL

.eia_url <- function(path){
  gsub("//", "/", file.path("https://api.eia.gov/v2/", path))
}

#' @importFrom httr GET content
.eia_get <- function(url){
  .antidos_before("eia")
  r <- httr::RETRY(verb = "GET", url = url, .session_eia_env$ua)
  .antidos_after("eia")
  if(r$status_code == "404") stop("Page not found", call. = FALSE)
  httr::content(r, as = "text", encoding = "UTF-8")
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
