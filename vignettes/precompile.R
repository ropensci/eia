# Pre-compiled vignettes that depend on API key
# Must manually move image files from eia/ to eia/vignettes/ after knit
knitr::knit("vignettes/api.Rmd.orig", "vignettes/api.Rmd")
knitr::knit("vignettes/eia.Rmd.orig", "vignettes/eia.Rmd")

