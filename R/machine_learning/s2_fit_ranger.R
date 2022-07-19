# PURPOSE: Demo random forests and gradient boosted trees


# Load the data -----------------------------------------------------------

library(tidyverse)
mlb_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/fg_batting_2022.csv") %>%
  janitor::clean_names() %>%
  mutate_at(vars(bb_percent:k_percent), parse_number)

model_mlb_data <- mlb_data %>%
  dplyr::select(-name, -team, -playerid)



# Fit random forests ------------------------------------------------------

library(ranger)
init_mlb_rf <-
  ranger(war ~ ., data = model_mlb_data,
         num.trees = 50, importance = "impurity")

library(vip)
vip(init_mlb_rf, geom = "col") + theme_bw()

init_mlb_rf$variable.importance

