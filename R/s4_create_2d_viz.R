# PURPOSE: Create 2D categorical viz with some inference


# Load data ---------------------------------------------------------------

library(tidyverse)
ohtani_batted_balls <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/ohtani_2021_batted_balls.csv")
head(ohtani_batted_balls)


# Explore pitch type variable ---------------------------------------------

table(ohtani_batted_balls$pitch_type)

ohtani_batted_balls <- ohtani_batted_balls %>%
  filter(!is.na(pitch_type)) %>%
  mutate(pitch_type = 
           fct_recode(pitch_type,
                      "Changeup" = "CH",
                      "Breaking ball" = "CU",
                      "Fastball" = "FC",
                      "Fastball" = "FF",
                      "Fastball" = "FS",
                      "Breaking ball" = "KC",
                      "Fastball" = "SI",
                      "Breaking ball" = "SL"))

table(ohtani_batted_balls$pitch_type)

chisq.test(table(ohtani_batted_balls$pitch_type))



# 2D viz with pitch type and batted ball type -----------------------------

ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type, fill = pitch_type)) +
  geom_bar() +
  ggthemes::scale_fill_colorblind() +
  theme_bw()

ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type, fill = pitch_type)) +
  geom_bar(position = "dodge") +
  ggthemes::scale_fill_colorblind() +
  theme_bw()


# Contigency tables -------------------------------------------------------

table("Pitch Type" = ohtani_batted_balls$pitch_type,
      "Batted ball type" = ohtani_batted_balls$batted_ball_type)

proportions(table("Pitch Type" = ohtani_batted_balls$pitch_type,
                  "Batted ball type" = ohtani_batted_balls$batted_ball_type))

table("Pitch Type" = ohtani_batted_balls$pitch_type,
      "Batted ball type" = ohtani_batted_balls$batted_ball_type) %>%
  proportions()


table("Pitch Type" = ohtani_batted_balls$pitch_type,
      "Batted ball type" = ohtani_batted_balls$batted_ball_type) %>%
  chisq.test()


# Mosaic ------------------------------------------------------------------


mosaicplot(table("Pitch Type" = ohtani_batted_balls$pitch_type,
                 "Batted ball type" = ohtani_batted_balls$batted_ball_type),
           main = "Relationship between batted ball type and pitch type?",
           shade = TRUE)

# Continuous by Categorical viz -------------------------------------------

ohtani_batted_balls %>%
  ggplot(aes(x = pitch_type, y = exit_velocity)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  theme_bw()

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity, color = pitch_type)) +
  stat_ecdf() +
  theme_bw() +
  theme(legend.position = "bottom")


ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity, fill = pitch_type)) +
  geom_histogram() +
  theme_bw() +
  theme(legend.position = "bottom")


ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity, fill = pitch_type)) +
  geom_histogram(position = "identity", alpha = .25) +
  theme_bw() +
  theme(legend.position = "bottom")


ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity, color = pitch_type)) +
  geom_histogram(fill = NA, position = "identity") +
  theme_bw() +
  theme(legend.position = "bottom")


ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(~ pitch_type, ncol = 2,
             scales = "free_y")

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(pitch_type ~ ., margins = TRUE)

