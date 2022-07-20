# PURPOSE: Multinomial logit regression and mixed effects models


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_ep_model_data <- read_rds(url("http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/model_pbp_data.rds"))

nfl_ep_model_data <- nfl_ep_model_data %>%
  mutate(Next_Score_Half = fct_relevel(Next_Score_Half, "No_Score"),
         # log transform of yards to go and indicator for two minute warning:
         log_ydstogo = log(ydstogo),
         # Changing down into a factor variable: 
         down = factor(down))



# Fit mulinomial logit model ----------------------------------------------

library(nnet)

init_ep_model <- 
  multinom(Next_Score_Half ~ half_seconds_remaining + yardline_100 + down + log_ydstogo + log_ydstogo*down + yardline_100*down,
           data = nfl_ep_model_data, maxit = 300)

summary(init_ep_model)


# Load CV results ---------------------------------------------------------

init_loso_cv_preds <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/init_nfl_ep_loso_cv_preds.csv")

ep_cv_loso_calibration <- init_loso_cv_preds %>%
  pivot_longer(No_Score:Touchdown,
               names_to = "next_score_type",
               values_to = "pred_prob") %>%
  mutate(bin_pred_prob = round(pred_prob / 0.05) * 0.05) %>%
  group_by(next_score_type, bin_pred_prob) %>%
  summarize(n_plays = n(),
            n_scoring_event = length(which(Next_Score_Half == next_score_type)),
            bin_actual_prob = n_scoring_event / n_plays,
            bin_se = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_plays)) %>%
  ungroup() %>%
  mutate(bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
         bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0))


ep_cv_loso_calibration %>%
  mutate(next_score_type = fct_relevel(next_score_type, "Opp_Safety", "Opp_Field_Goal", 
                                       "Opp_Touchdown", "No_Score", "Safety", "Field_Goal", "Touchdown"),
         next_score_type = fct_recode(next_score_type, "-Field Goal (-3)" = "Opp_Field_Goal", "-Safety (-2)" = "Opp_Safety", "-Touchdown (-7)" = "Opp_Touchdown",
                                      "Field Goal (3)" = "Field_Goal", "No Score (0)" = "No_Score",
                                      "Touchdown (7)" = "Touchdown", "Safety (2)" = "Safety")) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_smooth(se = FALSE) + 
  geom_point(aes(size = n_plays)) +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) + #coord_equal() +   
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of plays", x = "Estimated next score probability", 
       y = "Observed next score probability") + 
  theme_bw() + 
  theme(strip.background = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        legend.position = c(1, .05), legend.justification = c(1, 0)) +
  facet_wrap(~ next_score_type, ncol = 4)




# Load NFL passing plays --------------------------------------------------

nfl_passing_plays <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/nfl_passing_plays_2021.csv") %>%
  # Only keep rows with passer and receiver information known:
  filter(!is.na(passer_player_id), !is.na(receiver_player_id), !is.na(epa)) %>%
  # Combine passer and receiver unique IDs:
  mutate(passer_name_id = paste0(passer_player_name, ":", passer_player_id),
         receiver_name_id = paste0(receiver_player_name, ":", receiver_player_id))





