# Pre-compiled vignettes that depend on API key
# Must manually move image files from eia/ to eia/vignettes/ after knit

knitr::knit("vignettes/api.Rmd.orig", "vignettes/api.Rmd")
knitr::knit("vignettes/data.Rmd.orig", "vignettes/data.Rmd")
knitr::knit("vignettes/dirs.Rmd.orig", "vignettes/dirs.Rmd")
knitr::knit("vignettes/eia.Rmd.orig", "vignettes/eia.Rmd")

### DNE
# knitr::knit("vignettes/facets.Rmd.orig", "vignettes/facets.Rmd")
# knitr::knit("vignettes/metadata.Rmd.orig", "vignettes/metadata.Rmd")
