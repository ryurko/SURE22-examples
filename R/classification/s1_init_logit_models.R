# PURPOSE: Demonstrate logistic regression models for FGs


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_fg_attempts <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/glm_examples/nfl_fg_attempt_data.csv")
nfl_fg_attempts


# Fit logit model ---------------------------------------------------------

# poisson()
# binomial()

init_logit <- glm(is_fg_made ~ kick_distance, data = nfl_fg_attempts,
                  family = "binomial")

nfl_fg_attempts %>%
  mutate(pred_prob = init_logit$fitted.values) %>%
  ggplot(aes(x = kick_distance)) +
  geom_line(aes(y = pred_prob), color = "blue") +
  geom_point(aes(y = is_fg_made),
             alpha = 0.25, color = "darkorange") +
  theme_bw()


summary(init_logit)


head(init_logit$fitted.values)
summary(init_logit$fitted.values)

head(predict(init_logit, type = "response"))


pred_fg_outcome <- ifelse(init_logit$fitted.values > 0.5,
                          "make", "miss")


table("Predictions" = pred_fg_outcome,
      "Observed" = nfl_fg_attempts$is_fg_made)


# Calibration plots -------------------------------------------------------

nfl_fg_attempts %>%
  mutate(pred_prob = init_logit$fitted.values,
         bin_pred_prob = round(pred_prob / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(is_fg_made)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point(aes(size = n_attempts)) +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Estimated make probability",
       y = "Observed make probability",
       size = "Number of attempts") +
  theme_bw() +
  theme(legend.position = "bottom")






