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


# Perform 5-fold CV -------------------------------------------------------

set.seed(2020)
nfl_model_data <- nfl_model_data %>%
  mutate(test_fold = sample(rep(1:5, length.out = n())))


# Get cross-validation predictions
# INPUTS: model_formula, dataset (where dataset assumed to have test_fold)
# OUTPUT: Dataset containing test fold predictions

get_cv_preds <- function(model_formula, dataset = nfl_model_data) {
  
  map_dfr(unique(dataset$test_fold),
          function(holdout_i) {
            
            # Separate and get the training and test data
            test_data <- dataset %>%
              filter(test_fold == holdout_i)
            
            train_data <- dataset %>%
              filter(test_fold != holdout_i)
            
            # Fit the model on training data:
            train_model <-
              lm(as.formula(model_formula), data = train_data)
            
            # Return a table of holdout predictions:
            tibble(test_preds = predict(train_model, newdata = test_data),
                   test_actual = test_data$score_diff,
                   test_fold = holdout_i)
            
          })
  
}

# Get CV results with different sets of variables:
all_cv_preds <- get_cv_preds("score_diff ~  offense_ave_epa_pass + offense_ave_epa_run + defense_ave_epa_pass + defense_ave_epa_run")
all_int_cv_preds <- get_cv_preds("score_diff ~ offense_ave_epa_pass*offense_ave_epa_run + defense_ave_epa_pass*defense_ave_epa_run")
run_only_cv_preds <- get_cv_preds("score_diff ~ offense_ave_epa_run + defense_ave_epa_run")
pass_only_cv_preds <- get_cv_preds("score_diff ~ offense_ave_epa_pass + defense_ave_epa_pass")
off_only_cv_preds <- get_cv_preds("score_diff ~ offense_ave_epa_pass + offense_ave_epa_run")
def_only_cv_preds <- get_cv_preds("score_diff ~ defense_ave_epa_pass + defense_ave_epa_run")
int_only_cv_preds <- get_cv_preds("score_diff ~ 1")

bind_rows(mutate(all_cv_preds, type = "All"),
          mutate(all_int_cv_preds, type = "All w/ interactions"),
          mutate(pass_only_cv_preds, type = "Passing only"),
          mutate(run_only_cv_preds, type = "Running only"),
          mutate(off_only_cv_preds, type = "Offense only"),
          mutate(def_only_cv_preds, type = "Defense only"),
          mutate(int_only_cv_preds, type = "Intercept-only")) %>%
  group_by(type) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>%
  mutate(type = fct_reorder(type, rmse)) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip()

bind_rows(mutate(all_cv_preds, type = "All"),
          mutate(all_int_cv_preds, type = "All w/ interactions"),
          mutate(pass_only_cv_preds, type = "Passing only"),
          mutate(run_only_cv_preds, type = "Running only"),
          mutate(off_only_cv_preds, type = "Offense only"),
          mutate(def_only_cv_preds, type = "Defense only"),
          mutate(int_only_cv_preds, type = "Intercept-only")) %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>%
  mutate(type = fct_reorder(type, rmse)) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_point(alpha = .8) +
  theme_bw() +
  coord_flip()
