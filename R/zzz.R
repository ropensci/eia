.onAttach <- function(libname, pkgname) {
  options(eia_antidos = 1) # not sure yet what "antidos" is...
  key = Sys.getenv("EIA_KEY")
  x <- if(key == "") "EIA_KEY" else ""
  if(x == ""){
    packageStartupMessage("EIA_KEY found in Renviron.")
  } else {
    packageStartupMessage(
      x, " not found in Renviron.\n",
      "Please instantiate with eia::eia_set_key(<YOUR_KEY>), or...\n",
      "edit your Renviron file with usethis::edit_r_environ(), ",
      "then restart R."
    )
    if(identical(find.package("usethis", quiet = TRUE), character(0))) {
      packageStartupMessage("Install usethis with install.packages(\"usethis\").")
    }
  }
  invisible()
}

.session_eia_env <- new.env()
.session_eia_env$ua <- httr::user_agent("https://github.com/ropensci/eia")
