#Convert to PNG using magick
library(magick)




logo <- image_read("logo/logo.svg")

logo_png <- image_convert(logo, "png")
image_write(logo_png, "man/figures/logo.png", format = "png")


# Create smaller version for README
image_resize(logo, "120x139") |>
  image_write("man/figures/README-logo.png", format = "png")
