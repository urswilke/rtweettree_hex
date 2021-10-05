library(magrittr)
library(minisvg)

# library(tidyverse)
# # https://github.com/djnavarro/flametree
# ft <- flametree::flametree_grow(
#   seed = 123,
#   trees = 1,
#   time = 4,
#   angle = c(-30, -10, 0, 20,  30),
#   split = 5,
#   seg_wid = flametree::spark_decay(time = 0.3, multiplier = 2, constant = 0.1)
# )
# p_tree <- ft %>%
#   # flametree::flametree_plot(background = "white", palette = c("brown4", "forestgreen"))
#   ggplot()+
#   # code copied (with small adaptations) from Danielle Navarro's flametree
#   # package: https://github.com/djnavarro/flametree/blob/master/R/plot.R#L174 :
#   # |
#   # |
#   # v
#   ggforce::geom_bezier(
#     data = ft,
#     mapping = ggplot2::aes(
#       x = coord_x,          # x-coordinate
#       y = coord_y,          # y-coordinate
#       group = id_pathtree,  # each segment/path is a single bezier curve
#       size = seg_wid,       # the seg_wid variable is used to set line width
#       color = seg_col       # the seg_col variable is used to set line colour
#     ),
#     show.legend = FALSE,
#     lineend = "round"
#   ) +
#   ggplot2::scale_color_gradientn(colours = c("brown4", "forestgreen")) +
#   ggplot2::scale_size_identity() +
#   theme_void()
# ggsave("minisvg/tree2.svg", p_tree + theme(plot.margin = unit(c(5, 0, 0, 5), "cm")), width = 2200, height = 2200, units = "px",  bg = "transparent")









doc <- SVGDocument$new(width = 640, height = 640)

len     <- 320
angles  <- (seq(0, 360, 60) + 90) * pi/180
xs      <- round(len * cos(angles) + 320, 2)
ys      <- round(len * sin(angles) + 320, 2)
hex     <- stag$polygon(xs = xs, ys = ys)
hex_border     <- stag$polygon(xs = xs, ys = ys, stroke = "#94613d", fill_opacity=0, stroke_width = 15)

cp <- stag$clipPath(hex, id = 'cutOutHex')
g1 <- stag$linearGradient(
  id = "SkyGradient", x1="0", x2="0", y1="0", y2="1",
  stag$stop(offset = "0%", stop_color = "#32acf8"),
  stag$stop(offset = "50%", stop_color = "#7ccafa"),
  stag$stop(offset = "62%", stop_color = "#ffcd4f"),
  stag$stop(offset = "70%", stop_color = "#ff97f5")
)
g2 <- stag$radialGradient(
  id = "SunSurroundingGradient", cx="0.75", cy="0.63", r="0.25",
  stag$stop(offset = "0%", stop_color = "red"),
  stag$stop(offset = "100%", stop_color = "skyblue")
)
g3 <- stag$radialGradient(
  id = "HillGradient2", cx="0.5", cy="0.5", r="0.5",
  stag$stop(offset = "70%", stop_color = "#0de91a"),
  stag$stop(offset = "90%", stop_color = "green"),
  stag$stop(offset = "100%", stop_color = "#034006")
)
g4 <- stag$radialGradient(
  id = "RadialGradient3", cx="0.75", cy="0.63", r="0.7",
  stag$stop(offset = "0%", stop_color = "orange"),
  stag$stop(offset = "100%", stop_color = "slyblue")
)



doc$defs(g1, g2, g3, g4, cp)
rect_parameters <- list(x="0", y="0", rx="0", ry="0", width="640", height="640")
doc$append(
  stag$rect(
    rect_parameters,
    fill="white",
    fill_opacity="1",
    clip_path="url(#cutOutHex)"
  ),
  stag$rect(
    rect_parameters,
    fill="url(#SkyGradient)",
    fill_opacity="0.4",
    clip_path="url(#cutOutHex)"
  ),
  stag$circle(
    cx="480",
    cy="403",
    r="40",
    fill="#ffab00",
    fill_opacity="0.7",
    clip_path="url(#cutOutHex)"
  ),
  stag$rect(
    rect_parameters,
    fill="url(#SunSurroundingGradient)",
    fill_opacity="0.4",
    clip_path="url(#cutOutHex)"
  ),
  stag$rect(
    rect_parameters,
    fill="url(#RadialGradient3)",
    fill_opacity="0.4",
    clip_path="url(#cutOutHex)"
  ),
  stag$rect(
    rect_parameters,
    fill="url(#SkyGradient)",
    fill_opacity="0.4",
    clip_path="url(#cutOutHex)"
  ),
  stag$circle(
    cx="370",
    cy="1200",
    r="800",
    fill="url(#HillGradient2)",
    fill_opacity="1",
    clip_path="url(#cutOutHex)"
  )
)

# The svg of the tree was made with the code commented out on top of this script.
tree <- doc$image(
  href = "https://raw.githubusercontent.com/urswilke/rtweettree_hex/main/minisvg/tree2.svg",
  width = 640,
  x = -80,
  y = -80,
  clip_path="url(#cutOutHex)"
)

seagull <- doc$image(
  href = "http://www.clker.com/cliparts/e/9/1/3/11949854011768178445seagull_nicu_buculei_01.svg",
  width = 130,
  x = 25,
  y = 30,
  clip_path="url(#cutOutHex)"
)
seagull$animate(
  attributeName = 'x',
  keyTimes     = c(0, 1),
  values       = c(640, -70),
  dur          = 15,
  repeatCount = 'indefinite'
)
seagull$animate(
  attributeName = 'y',
  keyTimes     = seq(0, 1, length.out = 20),
  values       = 120 + 10 * sin(1:20) %>% round() + 1:20 * 2,
  dur          = 15,
  repeatCount = 'indefinite'
)
seagull$animate(
  attributeName = 'width',
  keyTimes     = seq(0, 1, length.out = 20),
  values       = seq(50, 130, length.out = 20) %>% round(),
  dur          = 15,
  repeatCount = 'indefinite'
)
doc$append(hex_border)
doc$show()
doc$save("minisvg/minisvg.svg")
