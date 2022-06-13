# PURPOSE: Density estimation viz


# Load data ---------------------------------------------------------------

library(tidyverse)
curry_shots <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/curry_2022_shots.csv")
head(curry_shots)


# Make histograms ---------------------------------------------------------


curry_shots %>%
  ggplot(aes(x = shot_distance)) +
  geom_histogram(breaks = seq(0, 50, by = 5)) +
  theme_bw()

# Noisy / spiky
curry_shots %>%
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 1) +
  theme_bw()

# Oversmooth / flat
curry_shots %>%
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 25) +
  theme_bw()

# Define more appropriate histogram breaks
curry_shots %>%
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 1, center = 0.5, closed = "left") +
  theme_bw()


# Density curves ----------------------------------------------------------

curry_shots %>%
  ggplot(aes(x = shot_distance)) +
  geom_density(adjust = 3) +
  geom_rug(alpha = 0.5) +
  theme_bw()

library(patchwork)
curry_density_curve <- curry_shots %>%
  ggplot(aes(x = shot_distance,
             color = is_shot_made)) +
  geom_density() +
  geom_rug(alpha = 0.5) +
  theme_bw()
curry_ecdf <- curry_shots %>%
  ggplot(aes(x = shot_distance,
             color = is_shot_made)) +
  stat_ecdf() +
  theme_bw()
curry_density_curve + curry_ecdf + plot_layout(guides = "collect")


curry_shots %>%
  ggplot(aes(x = shot_distance,
             color = is_shot_made)) +
  geom_density(alpha = .6) +
  geom_rug(alpha = 0.5) +
  theme_bw()

# Look at by shot type
curry_shots %>%
  ggplot(aes(x = shot_distance,
             color = shot_type)) +
  geom_density(alpha = .6) +
  geom_rug(alpha = 0.5) +
  theme_bw()

library(ggridges)
curry_shots %>%
  ggplot(aes(x = shot_distance, 
             y = shot_type)) +
  geom_density_ridges(rel_min_height = 0.01)


# 2D shot location density estimation ------------------------------------

curry_shots <- curry_shots %>%
  mutate(shot_x = -shot_x / 10,
         shot_y = shot_y / 10)

curry_shots %>%
  ggplot(aes(x = shot_x, y = shot_y)) +
  geom_point(alpha = 0.3) + 
  geom_density2d(adjust = 2) + 
  theme_bw() +
  coord_equal()

curry_shots %>%
  ggplot(aes(x = shot_x, y = shot_y)) +
  stat_density2d(geom = "polygon",
                 adjust = .75,
                 aes(fill = after_stat(level))) + 
  scale_fill_gradient(low = "darkblue", high = "darkorange") +
  #geom_point(alpha = 0.3) + 
  theme_bw() +
  coord_equal()

curry_shots %>%
  ggplot(aes(x = shot_x, y = shot_y)) +
  stat_density2d(geom = "raster",
                 contour = FALSE,
                 adjust = .75,
                 aes(fill = after_stat(density))) + 
  scale_fill_gradient(low = "white", high = "darkred") +
  #geom_point(alpha = 0.15, color = "white") + 
  theme_bw() +
  coord_equal()


curry_shots %>%
  ggplot(aes(x = shot_x, y = shot_y)) +
  geom_hex(binwidth = c(1, 1)) + 
  scale_fill_gradient(low = "darkblue", high = "darkorange") +
  theme_bw() +
  coord_equal()


curry_shots %>%
  ggplot(aes(x = shot_x, y = shot_y,
             z = is_shot_made, group = -1)) +
  stat_summary_hex(binwidth = c(1, 1),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue", high = "darkorange") +
  theme_bw() +
  theme(legend.position = "bottom")
  





