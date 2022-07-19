# PURPOSE: Use PCR and PLS models


# Load the data -----------------------------------------------------------

library(tidyverse)

# Read in as csv
#nfl_model_data <- read_csv("data/nfl_model_data.csv")

# Read in as rds
nfl_model_data <- read_rds("data/nfl_model_data.rds")


# Fit PCR -----------------------------------------------------------------

library(pls)

nfl_pcr_fit <- pcr(score_diff ~ ., ncomp = 2,
                   scale = TRUE, data = nfl_model_data)

summary(nfl_pcr_fit)


# Tune PCR with caret -----------------------------------------------------

library(caret)

set.seed(2013)
cv_model_pcr <- train(
  score_diff ~ .,
  data = nfl_model_data,
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10,
                           selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(nfl_model_data) - 1)

ggplot(cv_model_pcr) +
  theme_bw()

summary(cv_model_pcr$finalModel)



# Perform PLS -------------------------------------------------------------

set.seed(2013)
cv_model_pls <- train(
  score_diff ~ .,
  data = nfl_model_data,
  method = "pls",
  trControl = trainControl(method = "cv", number = 10,
                           selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(nfl_model_data) - 1)
ggplot(cv_model_pls) + theme_bw()
