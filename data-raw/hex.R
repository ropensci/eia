library(hexSticker)
library(ggplot2)

url <- "github.com/ropensci/eia"
out <- paste0("man/figures/logo.png")
dir.create("man/figures", showWarnings = FALSE)

hex_plot <- function(out, mult = 1){
  g <- ggplot() + theme_void() + theme_transparent()
  sticker(g, package = "eia", p_y = 1, p_color = "#427FD4", p_size = 40,
          h_color = "#427FD4", h_fill = "white", h_size =  1.4,
          url = url, u_color = "#427FD4", u_size = 3, filename = out)
}

hex_plot(out)
