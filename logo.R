library(ggplot2)
library(hexSticker)

p <-
  ggplot() +
  theme_void() +
  coord_fixed(xlim = c(-100, 100), ylim = c(-10, 10))

for (i in -6:5) {
  for (j in 1:8) {
    d <- i * 20 + j
    p <- p + geom_abline(intercept = d, slope = 0.6, color = "#e2261b")
  }
}

s <- sticker(
  subplot = p,
  s_width = 2.1,
  s_height = 1,
  s_y = 0.85,

  h_color = "#e2261b",
  h_fill = "#151515",

  package = "erify",
  p_size = 17,
  p_color = "#ffffff",
  p_y = 1.1,

  filename = "./man/figures/logo.png"
)
