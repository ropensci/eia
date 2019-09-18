#' Set and get API key
#'
#' @details
#' Setter and getter helpers allow you to store your EIA API key in one of three ways.
#' Their use is optional. You can always pass the API key string to the \code{key} argument of any package function that requires it,
#' but you do not have to.
#'
#' By default the \code{key} argument for these functions is \code{key = eia_get_key()}.
#' If your key has been stored in a manner that can be retrieved,
#' then you can call all the package API functions without having to provide the \code{key} argument repeatedly.
#'
#' @section Key storage methods:
#' If you have already set your key globally somewhere using \code{eia_set_key}, \code{eia_get_key} will retrieve it.
#' You can add the \code{EIA_KEY = "yourkey"} key-value pair to \code{options()} or as a system environment variable yourself and \code{eia_get_key}
#' will pick it up as long as you use the name \code{EIA_KEY}. For convenience you can do this in your R session with \code{eia_set_key}.
#' It gives you three options for how to store the key. The default is to use the \code{eia} package environment that is created when the package is loaded.
#'
#' @section Precedence:
#' Choose one method when setting a key. When getting the key, the three locations are checked in the order:
#' package environment, \code{options()}, then the system environment. To override the order, specify the method explicitly and the check will only occur there.
#' This also makes it possible to override a system level key by working with one stored in the package environment or \code{options()}.
#'
#' @section Persistence:
#' Note that none of these three storage methods, including \code{"sysenv"} are persistent; the stored key is lost when the R session is terminated.
#' A key that is stored outside of R as a system environment variable is retrievable with \code{eia_get_key},
#' just like those set in an R session with \code{eia_set_key} and \code{store = "sysenv"}.
#' However, if you truly want the key to persist as an environment variable when R terminates, you must manually add it somewhere like \code{.Renviron};
#' \code{Sys.setenv} in R cannot achieve this.
#'
#' @param key character, API key.
#' @param store character, method for storing API key. See details.
#'
#' @return \code{eia_get_key} returns the key string or \code{NULL} with a warning. \code{eia_set_key} returns a success message or an error.
#' @export
#' @name eia_key
#'
#' @examples
#' eia_set_key("fake")
#' eia_get_key()
#' # eia_get_key("options") returns an error if not set
eia_set_key <- function(key, store = c("env", "options", "sysenv")){
  store <- match.arg(store)
  err <- "Failed to set key."
  if(store == "env"){
    .session_eia_env$key <- key
    if(.session_eia_env$key == key){
      message("Key stored successfully in package environment.")
    } else {
      stop(err, call. = FALSE)
    }
  } else if(store == "options"){
    options(EIA_KEY = key)
    if(options()$EIA_KEY == key){
      message("Key stored successfully in options().")
    } else {
      stop(err, call. = FALSE)
    }
  } else {
    Sys.setenv(EIA_KEY = key)
    if(Sys.getenv("EIA_KEY") == key){
      message("Key stored successfully in system environment.")
    } else {
      stop(err, call. = FALSE)
    }
  }
  invisible()
}

#' @export
#' @rdname eia_key
eia_get_key <- function(store = c("env", "options", "sysenv")){
  store <- if(missing(store)) c("env", "options", "sysenv") else
    match.arg(store)
  if("env" %in% store){
    key <- .session_eia_env$key
    if(!is.null(key)) return(key)
  }
  if("options" %in% store){
    key <- options()$EIA_KEY
    if(!is.null(key)) return(key)
  }
  if("sysenv" %in% store){
    key <- Sys.getenv("EIA_KEY")
    if(!is.null(key) && key != "") return(key)
  }
  wrn <- paste("EIA API key not found in package environment,",
               "global options, or system enivronment variables.")
  warning(wrn, call. = FALSE)
  NULL
}
