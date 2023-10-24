.onAttach <- function(libname, pkgname){
  options(eia_antidos = 1)
  if(Sys.getenv("EIA_KEY") == ""){
    packageStartupMessage(
      "EIA_KEY not found. See `vignette(\"api\", \"eia\")` for key storage options."
    )
  }
}

.session_eia_env <- new.env()
.session_eia_env$ua <- httr::user_agent("https://github.com/ropensci/eia")
