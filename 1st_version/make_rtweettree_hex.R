library(dplyr)
library(ggplot2)
library(magick)

# https://github.com/djnavarro/flametree
ft <- flametree::flametree_grow(seed = 1234, trees = 1, time = 3, angle = c(-30, 30), split = 3)
p_tree <- ft %>%
  flametree::flametree_plot(background = "white", palette = c("brown", "green"))

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
  geom_path(color = "blue", size = 1.5) +
  coord_equal() +
  theme_void() +
  theme(plot.margin=grid::unit(c(0.2, 0.2, 0.2, 0.2), "mm"))


hex_png <- "1st_version/images/hex_contour.png"
ggsave(hex_png, p_hex, width = 320, height = 320, units = "px")
# black_hex_png <- "images/black_hex.png"
# ggsave(black_hex_png, p_black_hex, width = 320, height = 320, units = "px")
tree_png <- "1st_version/images/tree.png"
ggsave(tree_png, p_tree, width = 320, height = 320, units = "px")

mask <- image_read(hex_png)
bild <- image_read(tree_png) %>%
  image_resize("160x160")


eagle <- image_read_svg("images/bird_svgs/11949848182045168189eagle_01.svg", width = 30, height = 30)
kiwi <- image_read_svg("images/bird_svgs/11971203731299323252flomar_kiwi_(bird).svg", width = 30, height = 30)
amsel <- image_read_svg("images/bird_svgs/1194985418834879623uccello_profilo_01_archi_01.svg", width = 30, height = 30)
hex <- mask %>%
  image_composite(bild %>% image_scale("180x180"), "plus", offset = "+75+130") %>%
  image_composite(eagle, offset = "+190+140") %>%
  image_composite(mask) %>%
  image_composite(kiwi, offset = "+175+240") %>%
  image_composite(amsel %>% image_flop(), offset = "+75+150") %>%
  image_annotate("github.com/urswilke/rtweettree", size = 9, color = "skyblue",
                 degrees = 30, location = "+50+220") %>%
  image_annotate("rtweettree", size = 35, color = "blue",
                 location = "+80+90") %>%
  image_background("white")

image_write(hex, "1st_version/images/hex_sticker.png")

marg <- image_blank(160, 320) %>%
  image_background("white")

hex_and_margins_lr <- image_append(c(marg, hex, marg))
image_write(hex_and_margins_lr, "1st_version/images/hex_socialmedia_card.png")
