# PURPOSE: Fit models with regularization


# Load data ---------------------------------------------------------------

library(tidyverse)
nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")
nfl_model_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed) %>%
  # Only use rows with air yards
  filter(season >= 2006) %>%
  dplyr::select(-wins, -losses, -ties, -points_scored, -points_allowed, -season, -team)


# Begin using glmnet ------------------------------------------------------

library(glmnet)

# tidyverse way
model_x <- nfl_model_data %>%
  dplyr::select(-score_diff) %>%
  as.matrix()

# model.matrix way (dropping intercept)
model_x <- model.matrix(score_diff ~ 0 + ., nfl_model_data)

model_y <- nfl_model_data$score_diff
# model_y <- nfl_model_data %>%
#   pull(score_diff)

init_lm <- lm(score_diff ~ ., nfl_model_data)
library(broom)
tidy(init_lm) %>%
  filter(term != "(Intercept)") %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"),
                    guide = FALSE) +
  coord_flip() +
  theme_bw()

init_ridge_fit <- glmnet(model_x, model_y,
                         alpha = 0)
plot(init_ridge_fit, xvar = "lambda")


fit_ridge_cv <- cv.glmnet(model_x, model_y, alpha = 0)
plot(fit_ridge_cv)

fit_lasso_cv <- cv.glmnet(model_x, model_y, alpha = 1)
plot(fit_lasso_cv)

tidy_lasso_coef <- tidy(fit_lasso_cv$glmnet.fit)

tidy_lasso_coef %>%
  ggplot(aes(x = lambda, y = estimate,
             group = term)) +
  geom_line(alpha = 0.75) +
  scale_x_log10() + 
  geom_vline(xintercept = fit_lasso_cv$lambda.min,
             color = "darkblue", linetype = "dashed") +
  geom_vline(xintercept = fit_lasso_cv$lambda.1se,
             color = "darkred", linetype = "dashed") +
  theme_bw()

tidy_lasso_cv <- tidy(fit_lasso_cv)


# Elastic net examples ----------------------------------------------------

set.seed(2020)
test_fold_id <- sample(rep(1:10, length.out = nrow(model_x)))

cv_en_25 <- cv.glmnet(model_x, model_y, 
                      foldid = test_fold_id, alpha = .25)
cv_en_50 <- cv.glmnet(model_x, model_y, 
                      foldid = test_fold_id, alpha = .5)
cv_ridge <- cv.glmnet(model_x, model_y, 
                      foldid = test_fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_x, model_y, 
                      foldid = test_fold_id, alpha = 1)

which.min(c(min(cv_en_25$cvm), min(cv_en_50$cvm),
            min(cv_ridge$cvm), min(cv_lasso$cvm)))









