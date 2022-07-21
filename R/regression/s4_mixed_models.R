# PURPOSE: Fit mixed model for passing plays with player effects

library(tidyverse)

# Load NFL passing plays --------------------------------------------------

nfl_passing_plays <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/nfl_passing_plays_2021.csv") %>%
  # Only keep rows with passer and receiver information known:
  filter(!is.na(passer_player_id), !is.na(receiver_player_id), !is.na(epa)) %>%
  # Combine passer and receiver unique IDs:
  mutate(passer_name_id = paste0(passer_player_name, ":", passer_player_id),
         receiver_name_id = paste0(receiver_player_name, ":", receiver_player_id))


# Fit mixed / multilevel models -------------------------------------------

library(lme4)

passing_lmer <- lmer(epa ~ shotgun + air_yards + (1|passer_name_id) + (1|receiver_name_id),
                     data = nfl_passing_plays)

summary(passing_lmer)


VarCorr(passing_lmer) %>% 
  as_tibble() %>% 
  mutate(icc = vcov / sum(vcov)) %>% 
  dplyr::select(grp, icc)

library(merTools)
player_effects <- REsim(passing_lmer)
plotREsim(player_effects)


player_effects %>%
  as_tibble() %>%
  group_by(groupFctr) %>%
  arrange(desc(mean)) %>%
  slice(1:5, (n() - 4):n()) %>%
  ggplot(aes(x = reorder(groupID, mean))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - 2 * sd,
                    ymax = mean + 2 * sd)) +
  coord_flip() +
  facet_wrap(~groupFctr, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_bw()


