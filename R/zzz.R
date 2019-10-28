.onLoad <- function(libname, pkgname){
  options(eia_antidos = 1)
}

.session_eia_env <- new.env()
.session_eia_env$ua <- httr::user_agent("https://github.com/ropensci/eia")
