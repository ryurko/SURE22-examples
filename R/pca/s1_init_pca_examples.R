# PURPOSE: PCA on NFL teams data


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")
nfl_model_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed) %>%
  # Only use rows with air yards
  filter(season >= 2006) %>%
  dplyr::select(-wins, -losses, -ties, -points_scored, -points_allowed, -season, -team)



# Run PCA -----------------------------------------------------------------

model_x <- as.matrix(dplyr::select(nfl_model_data, -score_diff))
pca_nfl <- prcomp(model_x, center = TRUE, scale = TRUE)
summary(pca_nfl)

pca_nfl$sdev^2 / ncol(model_x)

library(broom)
pca_nfl %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1 / ncol(model_x),
             color = "red", linetype = "dashed") +
  theme_bw()
  

pca_nfl$x

library(factoextra)
fviz_eig(pca_nfl) +
  geom_hline(yintercept = (1 / ncol(model_x)) * 100,
             color = "red", linetype = "dashed")
  
fviz_pca_ind(pca_nfl)

fviz_pca_biplot(pca_nfl, label = "var", 
                alpha.ind = .5, col.var = "darkblue",
                alpha.var = .75)









