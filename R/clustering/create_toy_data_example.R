# PURPOSE: Make 2D clustering toy example

set.seed(2013)
cluster1 <- MASS::mvrnorm(1000, mu = c(0, 0), 
                          Sigma = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2))

cluster2 <- MASS::mvrnorm(1000, mu = c(.1, 0), 
                          Sigma = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2))
library(tidyverse)
as_tibble(cluster1) %>%
  bind_rows(as_tibble(cluster2)) %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point() +
  theme_bw()
  
fake_data <- as_tibble(cluster1) %>%
  mutate(cluster_label = "group1") %>%
  bind_rows(as_tibble(cluster2) %>% 
              mutate(cluster_label = "group2"))

fake_data %>%
  ggplot(aes(x = V1, y = V2, color = cluster_label)) +
  geom_point() +
  theme_bw()


fake_dist <- dist(dplyr::select(fake_data,
                                  V1, V2))

fake_dist_matrix <- as.matrix(fake_dist)

long_dist_matrix <- fake_dist_matrix %>%
  as_tibble() %>%
  mutate(obs1 = 1:nrow(fake_data)) %>%
  pivot_longer(cols = -obs1,
               names_to = "obs2", values_to = "distance")

long_dist_matrix %>%
  ggplot(aes(x = obs1, y = obs2, fill = distance)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange", high = "darkblue")

library(seriation)
fake_dist_seriate <- seriate(fake_dist)
player_order <- get_order(fake_dist_seriate)

player_names_order <-
  fake_data$player[player_order]


long_dist_matrix %>%
  mutate(player1 = fct_relevel(player1, player_names_order),
         player2 = fct_relevel(player2, player_names_order)) %>%
  ggplot(aes(x = player1, y = player2, fill = distance)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange", high = "darkblue")


