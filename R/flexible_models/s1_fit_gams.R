# PURPOSE: Demonstrate GAMs


# Load the data -----------------------------------------------------------

library(tidyverse)
batted_ball_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/mlb_batted_balls_2022.csv") %>%
  mutate(is_hr = as.numeric(events == "home_run")) %>%
  filter(!is.na(launch_angle), !is.na(launch_speed),
         !is.na(is_hr))
head(batted_ball_data)


# Use mgcv to model HR prob -----------------------------------------------

# Set-up training data
set.seed(2004)
batted_ball_data <- batted_ball_data %>%
  mutate(is_train = sample(rep(0:1, 
                               length.out = nrow(batted_ball_data))))

library(mgcv)
init_logit_gam <- 
  gam(is_hr ~ s(launch_speed) + s(launch_angle),
      data = filter(batted_ball_data, is_train == 1),
      family = binomial, method = "REML")

library(gratia)
draw(init_logit_gam, fun = plogis,
     constant = coef(init_logit_gam)[1])

gam.check(init_logit_gam)

batted_ball_data <- batted_ball_data %>%
  mutate(init_gam_hr_prob =
           as.numeric(predict(init_logit_gam,
                   newdata = batted_ball_data,
                   type = "response")),
         init_gam_hr_class = as.numeric(init_gam_hr_prob >= 0.5))#,
#         init_gam_hr_class = ifelse(init_gam_hr_prob >= 0.5, 1, 0))


batted_ball_data %>%
  group_by(is_train) %>%
  summarize(correct = mean(is_hr == init_gam_hr_class))

init_linear_logit <-
  glm(is_hr ~ launch_speed + launch_angle,
      data = filter(batted_ball_data, is_train == 1),
      family = binomial)

batted_ball_data <- batted_ball_data %>%
  mutate(init_logit_hr_prob =
           as.numeric(predict(init_linear_logit,
                              newdata = batted_ball_data,
                              type = "response")),
         init_logit_hr_class = as.numeric(init_logit_hr_prob >= 0.5))#,

batted_ball_data %>%
  group_by(is_train) %>%
  summarize(correct = mean(is_hr == init_logit_hr_class))

multi_logit_gam <-
  gam(is_hr ~ s(launch_speed, launch_angle),
      data = filter(batted_ball_data, is_train == 1),
      family = binomial)
draw(multi_logit_gam)

batted_ball_data <- batted_ball_data %>%
  mutate(multi_logit_hr_prob =
           as.numeric(predict(multi_logit_gam,
                              newdata = batted_ball_data,
                              type = "response")),
         multi_logit_hr_class = as.numeric(multi_logit_hr_prob >= 0.5))#,

batted_ball_data %>%
  group_by(is_train) %>%
  summarize(correct = mean(is_hr == multi_logit_hr_class))

