.onAttach <- function(libname, pkgname) {
  options(eia_antidos = 1) # not sure yet what "antidos" is...
  key = Sys.getenv("EIA_KEY")
  if(key == "") {
    x <- "EIA_KEY"
  } else {
    x <- ""
  }
  if(x == ""){
    packageStartupMessage("EIA_KEY found in Renviron.")
  } else {
    packageStartupMessage(
      x, " not found in Renviron. Please add and restart R.\n",
      "You can edit your Renviron file with usethis::edit_r_environ()."
    )
    if(identical(find.package("usethis", quiet = TRUE), character(0))) {
      packageStartupMessage("Install usethis with install.packages(\"usethis\").")
    }
  }
  invisible()
}

.session_eia_env <- new.env()
.session_eia_env$ua <- httr::user_agent("https://github.com/ropensci/eia")
