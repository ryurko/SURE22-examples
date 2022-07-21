# Read in NFL data going back to 1999

nfl_pbp_data <- nflreadr::load_pbp(seasons = TRUE, file_type = "rds")

clean_nfl_pbp_data <- nfl_pbp_data %>%
  mutate(defteam = ifelse(is.na(defteam), "NOTHING", defteam),
         timeout_team = ifelse(is.na(timeout_team), "NOTHING", timeout_team)) %>%
  group_by(game_id) %>%
  mutate(prev_def_team_to = as.numeric(defteam != "NOTHING") * 
           as.numeric(defteam == lag(timeout_team)),
         prev_def_team_to = ifelse(is.na(prev_def_team_to), 0,
                                   prev_def_team_to)) %>%
  ungroup() %>%
  mutate(is_iced_kick = prev_def_team_to * field_goal_attempt)

fg_data <- clean_nfl_pbp_data %>%
  filter(field_goal_attempt == 1)

 