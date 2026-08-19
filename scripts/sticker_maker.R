library(hexSticker)
library(magick)
library(ggplot2)

# ------------------------------------------------------------
# File locations
# ------------------------------------------------------------

character_file <- "assets/scrooge-character.png"
title_file     <- "assets/scrooge-mcduck-title.png"
centre_file    <- "assets/scrooge-mcduck-centre.png"
sticker_file   <- "assets/scrooge-mcduck-sticker.png"

stopifnot(
  file.exists(character_file),
  file.exists(title_file)
)

# ------------------------------------------------------------
# Prepare the duck
# ------------------------------------------------------------

duck <- image_read(character_file) |>
  image_trim() |>
  image_resize("550x750")

# Magick preserves the duck's original proportions because
# the resize geometry does not contain an exclamation mark.

# ------------------------------------------------------------
# Prepare the title
# ------------------------------------------------------------

title <- image_read(title_file) |>
  image_trim() |>
  image_resize("600x310")

# ------------------------------------------------------------
# Build the central composition
# ------------------------------------------------------------

canvas_width  <- 1300
canvas_height <- 900

centre_design <- image_blank(
  width = canvas_width,
  height = canvas_height,
  color = "transparent"
)

# Duck on the left
centre_design <- image_composite(
  centre_design,
  duck,
  operator = "over",
  offset = "+250+60"
)

# Title underneath the hand holding the money
centre_design <- image_composite(
  centre_design,
  title,
  operator = "over",
  offset = "+500+375"
)

# Save the combined duck-and-title design
image_write(
  centre_design,
  path = centre_file,
  format = "png"
)

# Preview the combined design
print(centre_design)

# ------------------------------------------------------------
# Prepare the currency-symbol background
# ------------------------------------------------------------

currency_pattern <- expand.grid(
  x = seq(0.12, 0.88, length.out = 10),
  y = seq(0.08, image_ratio - 0.1, length.out = 8)
)

currency_pattern$label <- rep(
  c("€", "$", "¥", "£"),
  length.out = nrow(currency_pattern)
)

currency_pattern$angle <- rep(
  c(-12, 8, -6, 12, 0, -8, 6, -4),
  length.out = nrow(currency_pattern)
)

currency_pattern$symbol_size <- rep(
  c(7, 8, 9, 8, 7, 9, 8, 7),
  length.out = nrow(currency_pattern)
)

# Identify the different rows
row_number <- match(
  currency_pattern$y,
  sort(unique(currency_pattern$y))
)

# Stagger alternate rows
currency_pattern$x <- currency_pattern$x +
  ifelse(row_number %% 2 == 0, 0.025, 0)

# Keep every symbol inside the background canvas
currency_pattern <- subset(
  currency_pattern,
  x >= 0.10 & x <= 0.90
)
# ------------------------------------------------------------
# Create the central ggplot
# ------------------------------------------------------------

centre_plot <- ggplot() +

  # Currency symbols are drawn first, so they remain in the background
  geom_text(
    data = currency_pattern,
    aes(
      x = x,
      y = y,
      label = label,
      angle = angle,
      size = symbol_size
    ),
    colour = "#4C979F",
    alpha = 0.30,
    family = "serif",
    fontface = "bold",
    show.legend = FALSE
  ) +

  scale_size_identity() +

  # Duck and title are drawn on top of the currency symbols
  annotation_raster(
    raster = as.raster(centre_design),
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = image_ratio,
    interpolate = TRUE
  ) +

  coord_fixed(
    ratio = 1,
    xlim = c(0, 1),
    ylim = c(0, image_ratio),
    expand = FALSE,
    clip = "off"
  ) +

  theme_void() +

  theme(
    plot.margin = margin(0, 0, 0, 0),

    plot.background = element_rect(
      fill = "transparent",
      colour = NA
    ),

    panel.background = element_rect(
      fill = "transparent",
      colour = NA
    )
  )

# Preview the complete central composition
centre_plot

# ------------------------------------------------------------
# Create the final sticker
# ------------------------------------------------------------

design_width <- 2.25

scrooge_sticker <- sticker(
  subplot = centre_plot,

  # No hexSticker title:
  # the designed title is already inside centre_plot
  package = "",

  # Main composition
  s_x = 1,
  s_y = 0.98,
  s_width = design_width,
  s_height = design_width * image_ratio,

  # Hexagon styling
  h_fill = "#FFEAB0",
  h_color = "#9f4c4c",
  h_size = 2.5,

  # Background highlight
  spotlight = TRUE,
  l_x = 0.82,
  l_y = 1.08,
  l_width = 1.7,
  l_height = 1.7,
  l_alpha = 0.32,


  white_around_sticker = FALSE,
  filename ="assets/scrooge-mcduck-web-logo.png",
  dpi = 600
)

plot(scrooge_sticker)

