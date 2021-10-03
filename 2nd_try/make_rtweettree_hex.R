library(dplyr)
library(ggplot2)
library(magick)

# https://github.com/djnavarro/flametree
ft <- flametree::flametree_grow(seed = 123, trees = 1, time = 4, angle = c(-30, -10, 0, 20,  30), split = 5)
p_tree <- ft %>%
  flametree::flametree_plot(background = "white", palette = c("brown4", "forestgreen")) +
  # https://stackoverflow.com/a/41878833
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

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
  # geom_polygon(fill = "white") +
  geom_path(color = "blue", size = 1.5 * 2) +
  coord_equal() +
  theme_void() +
  theme(plot.margin=grid::unit(c(0.2, 0.2, 0.2, 0.2) * 2, "mm"))

p_black_hex <- df_hexagon %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(fill = "black") +
  coord_equal() +
  theme_void() +
  theme(plot.margin=grid::unit(c(0.2, 0.2, 0.2, 0.2) * 2, "mm"))

hex_png <- "images/hex_contour.png"
black_hex_png <- "images/black_hex.png"
fig_size <- 320
ggsave(hex_png, p_hex, width = fig_size * 2, height = fig_size * 2, units = "px")
ggsave(black_hex_png, p_black_hex, width = fig_size * 2, height = fig_size * 2, units = "px")
# black_hex_png <- "images/black_hex.png"
# ggsave(black_hex_png, p_black_hex, width = fig_size, height = fig_size, units = "px")
tree_png <- "images/tree.png"
ggsave(tree_png, p_tree, width = fig_size * 4, height = fig_size * 4, units = "px",  bg = "transparent")

mask <- image_read(hex_png) %>%
  image_scale(geometry_size_pixels(135 * 2, 135 * 2))
bild <- image_read(tree_png) %>%
  image_resize(geometry_area(fig_size * 2, fig_size * 2))


eagle <- image_read_svg("images/bird_svgs/11949848182045168189eagle_01.svg", width = 30, height = 30)
kiwi <- image_read_svg("images/bird_svgs/11971203731299323252flomar_kiwi_(bird).svg", width = 30, height = 30)
amsel <- image_read_svg("images/bird_svgs/1194985418834879623uccello_profilo_01_archi_01.svg", width = 30, height = 30)
bg <- image_read_svg("images/bird_svgs/12065578651282111944chelseafan528_Grassy_Scene.svg", width = 320, height = 320) %>% image_crop("400x400+50+50")
black_mask <- image_read(black_hex_png) %>% image_scale(geometry_area(270, 270))

bg_hex <- image_composite(black_mask, bg)

hex <-
  image_composite(bg_hex, mask, "over") %>%
  image_composite(
    bild %>% image_scale(geometry_size_pixels(180, 180)),
    "over",
    offset = geometry_point(45, 60)
  ) %>%
  # bild %>% #image_scale(geometry_size_pixels(180 * 2, 180 * 2)) %>%
  image_composite(mask) %>%
  image_composite(eagle, "over", offset = geometry_point(160, 60)) %>%
  image_composite(kiwi, "over", offset = geometry_point(140, 200)) %>%
  image_composite(amsel %>% image_flop(), "over", offset = geometry_point(75, 60)) %>%
  image_annotate("github.com/urswilke/rtweettree", size = 8, color = "blue",
                 degrees = 30, location = geometry_point(42, 186)) %>%
  image_annotate("rtweettree", size = 35, color = "blue",
                 location = geometry_point(60, 90)) %>%
  image_background("white")

image_write(hex, "images/hex_sticker.png")

marg <- image_blank(160 * 2, fig_size * 2) %>%
  image_background("white")

hex_and_margins_lr <- image_append(c(marg, hex, marg))
image_write(hex_and_margins_lr, "images/hex_socialmedia_card.png")
