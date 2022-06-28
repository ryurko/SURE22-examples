# PURPOSE: Explore variable correlations


# Load data ---------------------------------------------------------------

library(tidyverse)
nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")
nfl_teams_data


nfl_teams_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed)


hist(nfl_teams_data$score_diff)

library(ggcorrplot)
nfl_model_data <- nfl_teams_data %>%
  dplyr::select(score_diff, offense_ave_epa_pass,
                offense_ave_epa_run, 
                defense_ave_epa_pass,
                defense_ave_epa_run,
                offense_ave_yards_gained_pass,
                offense_ave_yards_gained_run,
                defense_ave_yards_gained_pass,
                defense_ave_yards_gained_run)
nfl_cor_matrix <- round(cor(nfl_model_data), digits = 2)
ggcorrplot(nfl_cor_matrix, type = "lower", lab = TRUE,
           hc.order = TRUE)


nfl_ex_vars <- dplyr::select(nfl_model_data, -score_diff)
ex_cor_matrix <- cor(nfl_ex_vars)
cor_dist_matrix <- 1 - abs(ex_cor_matrix)
cor_dist_matrix <- as.dist(cor_dist_matrix)

nfl_ex_hc <- hclust(cor_dist_matrix, "complete")
library(ggdendro)
ggdendrogram(nfl_ex_hc, rotate = TRUE, size = 2)

library(GGally)
ggpairs(nfl_model_data,
        columns = c("score_diff", "offense_ave_epa_run",
                    "offense_ave_epa_pass", 
                    "defense_ave_epa_run", 
                    "defense_ave_epa_pass"),
        aes(alpha = .5))






