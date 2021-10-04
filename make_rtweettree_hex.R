library(dplyr)
library(ggplot2)
library(magick)
library(ggpattern)



# tree with hill and sky background: --------------------------------------

# https://github.com/djnavarro/flametree
ft <- flametree::flametree_grow(
  seed = 123,
  trees = 1,
  time = 4,
  angle = c(-30, -10, 0, 20,  30),
  split = 5,
  seg_wid = flametree::spark_decay(time = 0.3, multiplier = 1, constant = 0.1)
)

# https://stackoverflow.com/a/6863490
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(tibble(x = xx, y = yy))
}

xlims <- c(floor(min(ft$coord_x)) - 1, ceiling(max(ft$coord_x)) + 1)
ylims <- c(floor(min(ft$coord_y)) - 1, ceiling(max(ft$coord_y)) + 1)

hill <- circleFun(c(0,-5), 12, npoints = 1000) %>%
  filter(x >= xlims[1] - 1.5 & x <= xlims[2] + 1.5) %>%
  filter(y >= ylims[1] - 1.5 & y <= ylims[2] + 1.5) %>%
  mutate(
    type = "hill",
    pattern_fill  = "green",
    pattern_fill2 = "black",
    pattern_orientation = "horizontal"
  )
sky = tibble(
  xmin = xlims[1], xmax = xlims[2],
  ymin = ylims[1], ymax = ylims[2],
  pattern_fill  = "white",
  pattern_fill2 = "skyblue",
  type = "sky",
  pattern_orientation = "vertical"
)


# modify the color & width of the bezier branches to change gradually to the one
# of the child branches:
ft_mod <- ft %>%
  group_by(coord_x, coord_y) %>%
  mutate(n = n()) %>%
  mutate(
    seg_wid = ifelse(n > 1, min(seg_wid), seg_wid),
    seg_col = ifelse(n > 1, max(seg_col), seg_col)
    ) %>%
  ungroup() %>%
  select(-n)

# This creates a small white frame around the ggplot:
# p_tree <- ft_mod %>%
#   flametree::flametree_plot(background = "white", palette = c("brown4", "forestgreen"))

# Therefore, I took the code from the internal function that is called by
# flametree::flametree_plot() (see comment below):


p_tree <- ggplot() +
  geom_rect_pattern(data = sky,
                    aes(
                      xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                      pattern_fill  = I(pattern_fill),
                      pattern_fill2 = I(pattern_fill2),
                      pattern_orientation = I(pattern_orientation)
                    ),
                    pattern         = 'gradient',
                    # colour          = 'black',
                    pattern_density = 0.3
  ) +
  geom_polygon_pattern(data = hill,
                       aes(
                         x=x,
                         y = y,
                         pattern_fill  = I(pattern_fill),
                         pattern_fill2 = I(pattern_fill2),
                         pattern_orientation = I(pattern_orientation)
                       ),
                       pattern         = 'gradient',
                       # colour          = 'black',
                       pattern_density = 0.3
  ) +
  # code copied (with small adaptations) from Danielle Navarro's flametree
  # package: https://github.com/djnavarro/flametree/blob/master/R/plot.R#L174 :
  # |
  # |
  # v
  ggforce::geom_bezier2(
    data = ft_mod,
    mapping = ggplot2::aes(
      x = coord_x,          # x-coordinate
      y = coord_y,          # y-coordinate
      group = id_pathtree,  # each segment/path is a single bezier curve
      size = seg_wid,       # the seg_wid variable is used to set line width
      color = seg_col       # the seg_col variable is used to set line colour
    ),
    show.legend = FALSE,
    lineend = "round"
    ) +
  ggplot2::scale_color_gradientn(colours = c("brown4", "forestgreen")) +
  ggplot2::scale_size_identity() +
  # ^
  # |
  # |
  # until here: code copied from Danielle Navarro's flametree
  # package: https://github.com/djnavarro/flametree/blob/master/R/plot.R#L174
  # ggplot2::coord_equal() +
  coord_cartesian(xlims, ylims, expand = FALSE) +
  ggplot2::theme_void()



# hex shapes: -------------------------------------------------------------

reg_polygon <- function(n, start = 0i) {
  z <- exp(0:(n-1) * 2i / n * pi + start)
  df <- data.frame(x = Re(z), y = Im(z))
  bind_rows(df, df[1,])
  # z
}
df_hexagon <- reg_polygon(6, 1i * pi / 6) %>%
  # HACK to "close the path" (without, there is a small gap at one corner):
  bind_rows(slice(., 1:2))


p_hex <- df_hexagon %>%
  ggplot(aes(x = x, y = y)) +
  geom_path(color = "blue", size = 1.5 * 2) +
  coord_equal() +
  theme_void()

p_black_hex <- df_hexagon %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(fill = "black") +
  coord_equal() +
  theme_void()



# export to png: ----------------------------------------------------------

fig_size <- 320
hex_png <- "images/hex_contour.png"
black_hex_png <- "images/black_hex.png"
ggsave(hex_png, p_hex, width = fig_size * 2, height = fig_size * 2, units = "px")
ggsave(black_hex_png, p_black_hex, width = fig_size * 2, height = fig_size * 2, units = "px")
tree_png <- "images/tree.png"
ggsave(tree_png, p_tree, width = fig_size * 2, height = fig_size * 2, units = "px",  bg = "transparent")




# import to magick: -------------------------------------------------------

hex_border <- image_read(hex_png) %>%
  image_scale(geometry_size_pixels(fig_size * 2, fig_size * 2))
bild <- image_read(tree_png)


parrot <- image_read_svg("images/bird_svgs/11971251681733865363emeza_Guacamaya.svg", width = 45, height = 45)
oyster_catcher <- image_read_svg("images/bird_svgs/1197156164755347512johnny_automatic_oyster_catcher.svg", width = 45, height = 45)
seagull <- image_read_svg("images/bird_svgs/11949854011768178445seagull_nicu_buculei_01.svg", width = 30 * 2, height = 30 * 2)
finch <- image_read_svg("images/bird_svgs/11971499341074136672MichowwTru_Gold_Finch.svg", width = 30, height = 30)
magpie <- image_read_svg("images/bird_svgs/1195441923684948462zeimusu_Magpie.svg", width = 30 * 2, height = 30 * 2)
hummingbird <- image_read_svg("images/bird_svgs/12065563241286133361Chrisdesign_Hummingbird.svg", width = 30, height = 30)
black_mask <- image_read(black_hex_png) %>% image_scale(geometry_area(fig_size * 2, fig_size * 2))


# compose image of different components: ----------------------------------

hex <-
  # cut out hexagon of background image:
  image_composite(black_mask, bild) %>%
  # add hex border:
  image_composite(hex_border, "over") %>%
  # add text:
  image_annotate("github.com/urswilke/rtweettree", size = 20, color = "black",
                 degrees = 30, location = geometry_point(95, 435)) %>%
  image_annotate("rtweettree", size = 90, color = "white", weight = 800,
                 location = geometry_point(103, 190)) %>%
  # add birds:
  image_composite(parrot, "over", offset = geometry_point(460, 160)) %>%
  image_composite(oyster_catcher %>% image_flop(), "over", offset = geometry_point(340, 450)) %>%
  image_composite(finch, "over", offset = geometry_point(440, 290)) %>%
  image_composite(hummingbird, "over", offset = geometry_point(350, 350)) %>%
  image_composite(seagull, "over", offset = geometry_point(275, 70)) %>%
  image_composite(magpie, "over", offset = geometry_point(340, 150)) %>%
  # change background from transparent to white:
  image_background("white")


# add white margins left & right:
marg <- image_blank(fig_size, fig_size * 2) %>%
  image_background("white")
hex_and_margins_lr <- image_append(c(marg, hex, marg))



# save final images: ------------------------------------------------------

image_write(hex, "images/hex_sticker.png")

image_write(hex_and_margins_lr, "images/hex_socialmedia_card.png")
