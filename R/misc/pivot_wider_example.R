library(tidyverse)
nfl_fg_attempts <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/glm_examples/nfl_fg_attempt_data.csv")

nfl_fgs <- nfl_fg_attempts %>%
  dplyr::select(kicker_player_name,
                posteam, qtr, is_fg_made,
                kick_distance) %>%
  group_by(kicker_player_name, posteam, qtr) %>%
  summarize(fg_perc = mean(is_fg_made, na.rm = TRUE),
            ave_kick_dist = mean(kick_distance, na.rm = TRUE),
            .groups = "drop") 

nfl_wide_fgs <- nfl_fgs %>%
  pivot_wider(id_cols = c(kicker_player_name, posteam),
              names_from = qtr,
              names_glue = "qtr{qtr}_{.value}",
              values_from = c(fg_perc, ave_kick_dist))

nfl_wide_fgs <- nfl_fgs %>%
  pivot_wider(names_from = qtr,
              names_glue = "qtr{qtr}_{.value}",
              values_from = fg_perc:ave_kick_dist)

