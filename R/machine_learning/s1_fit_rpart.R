# PURPOSE: Explore modeling with decision trees


# Load the data -----------------------------------------------------------

library(tidyverse)
mlb_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/fg_batting_2022.csv") %>%
  janitor::clean_names() %>%
  mutate(across(bb_percent:k_percent, parse_number))
  #mutate_at(vars(bb_percent:k_percent), parse_number)
head(mlb_data)


# Regression tree example -------------------------------------------------

library(rpart)
init_mlb_tree <- rpart(w_oba ~ bb_percent + k_percent + iso,
                       data = mlb_data, method = "anova")

init_mlb_tree


library(rpart.plot)
rpart.plot(init_mlb_tree)


plotcp(init_mlb_tree)

full_mlb_tree <- rpart(w_oba ~ bb_percent + k_percent + iso,
                       data = mlb_data, method = "anova",
                       control = list(cp = 0, xval = 10))
rpart.plot(full_mlb_tree)
plotcp(full_mlb_tree)



# Caret example -----------------------------------------------------------

library(caret)
caret_mlb_tree <-
  train(w_oba ~ bb_percent + k_percent + iso + avg + obp + slg + war,
        data = mlb_data, method = "rpart",
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 20)

ggplot(caret_mlb_tree) + theme_bw()

rpart.plot(caret_mlb_tree$finalModel)

library(vip)
vip(caret_mlb_tree, geom = "point") + theme_bw()

library(pdp)
partial(caret_mlb_tree, pred.var = "obp") %>% autoplot()
