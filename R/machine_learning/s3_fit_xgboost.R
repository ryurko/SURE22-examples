# PURPOSE: Demo XGBoost


# Load the data -----------------------------------------------------------

library(tidyverse)
mlb_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/fg_batting_2022.csv") %>%
  janitor::clean_names() %>%
  mutate_at(vars(bb_percent:k_percent), parse_number)

model_mlb_data <- mlb_data %>%
  dplyr::select(-name, -team, -playerid)



# Fitting XGBoost ---------------------------------------------------------

library(xgboost)

xgboost_tune_grid <- expand.grid(nrounds = seq(from = 20, to = 200, by = 20),
                                 eta = c(0.025, 0.05, 0.1, 0.3),
                                 gamma = 0,
                                 max_depth = c(1, 2, 3, 6),
                                 colsample_bytree = 1,
                                 min_child_weight = 1,
                                 subsample = 1)
library(caret)
xgboost_tune_control <- trainControl(method = "cv", number = 5, 
                                     verboseIter = FALSE)

xgb_tune <- train(x = as.matrix(dplyr::select(model_mlb_data, -war)),
                  y = model_mlb_data$war,
                  trControl = xgboost_tune_control,
                  tuneGrid = xgboost_tune_grid,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  verbose = FALSE)



xgb_tune$finalModel


xgb_fit_final <- xgboost(data = as.matrix(dplyr::select(model_mlb_data, -war)),
                         label = model_mlb_data$war,
                         objective = "reg:squarederror",
                         nrounds = xgb_tune$bestTune$nrounds,
                         params = 
                           as.list(dplyr::select(xgb_tune$bestTune, -nrounds)))

library(vip)
vip(xgb_fit_final) + theme_bw()
