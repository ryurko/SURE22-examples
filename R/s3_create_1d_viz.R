# PURPOSE: Exploring 1D categorical and continuous variables


# Load the data -----------------------------------------------------------

library(tidyverse)
ohtani_batted_balls <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/ohtani_2021_batted_balls.csv")
head(ohtani_batted_balls)


# Make a bar chart of batted ball type --------------------------------------------

ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar() +
  theme_bw()

ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  theme_bw()

ohtani_batted_balls %>%
  group_by(batted_ball_type) %>%
  summarize(count = n(), .groups = "drop") %>%
  #ungroup() %>%
  mutate(total = sum(count),
         prop = count / total,
         se = sqrt(prop * (1 - prop) / total),
         lower = prop - 2 * se,
         upper = prop + 2 * se,
         batted_ball_type =
           fct_reorder(batted_ball_type, prop)) %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar(aes(y = prop), stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                color = "red") +
  theme_bw()
  

# Visualize exit velo -----------------------------------------------------

ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity)) +
  geom_boxplot(aes(x = "")) +
  theme_bw() +
  coord_flip()

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) +
  geom_histogram(bins = 30) +
  theme_bw()


library(ggbeeswarm)
ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity)) +
  geom_beeswarm(aes(x = ""),
                cex = 3) +
  theme_bw() +
  coord_flip()

ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity,
             x = "")) +
  geom_violin() +
  geom_boxplot(width = .2) +
  coord_flip() +
  theme_bw()

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) +
  stat_ecdf() +
  geom_rug(alpha = .3) +
  theme_bw()


ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity, y = launch_angle)) +
  geom_point(alpha = .45) +
  geom_rug(alpha = .3) +
  theme_bw()



