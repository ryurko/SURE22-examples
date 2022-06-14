# PURPOSE: Exploring the Lahman Batting dataset

# Load packages ----

library(tidyverse)
library(Lahman)

# Iniitial examination of data ----

Batting <- as_tibble(Batting)

dim(Batting)
class(Batting)

head(Batting)
tail(Batting)

colnames(Batting)


# Begin summarizing Batting -----------------------------------------------

summary(Batting$yearID)

table(Batting$lgID)

any(is.na(Batting$lgID))

mlb_batting <- filter(Batting, lgID %in% c("AL", "NL"))

sel_batting <- dplyr::select(Batting, yearID, lgID, G, AB, R, H, HR, BB, SO)

arrange(Batting, HR)

summarize(Batting, max(stint), median(AB))

new_batting <- mutate(Batting, batting_avg = H / AB)


head(arrange(select(mutate(Batting, BA = H / AB), playerID, BA), desc(BA)), 
     n = 1)

Batting %>%
  filter(lgID %in% c("AL", "NL"),
         AB > 300) %>%
  # Compute batting avg
  mutate(`Batting AVG` = H / AB) %>% 
  arrange(desc(`Batting AVG`)) %>%
  dplyr::select(playerID, yearID, `Batting AVG`) %>%
  slice(c(1, 2, 5, 42, 183))

Batting %>%
  filter(lgID %in% c("AL", "NL"),
         AB > 300) %>%
  group_by(yearID) %>%
  summarize(hr = sum(HR), so = sum(SO), bb = sum(BB)) %>%
  arrange(desc(hr)) %>%
  slice(1:5)

Batting %>%
  filter(lgID %in% c("AL", "NL"),
         AB > 300) %>%
  group_by(yearID, playerID) %>%
  summarize(hr = sum(HR), so = sum(SO), bb = sum(BB),
            .groups = "drop") %>%
  arrange(desc(hr)) %>%
  slice(1:5)


# Create year batting summary ---------------------------------------------
year_batting_summary <- Batting %>%
  filter(lgID %in% c("AL", "NL")) %>%
  group_by(yearID) %>%
  summarize(total_hits = sum(H, na.rm = TRUE),
            total_hrs = sum(HR, na.rm = TRUE),
            total_ks = sum(SO, na.rm = TRUE),
            total_walks = sum(BB, na.rm = TRUE),
            total_abs = sum(AB, na.rm = TRUE)) %>%
  mutate(batting_avg = total_hits / total_abs) 

library(gt)
year_batting_summary %>%
  dplyr::select(yearID, batting_avg) %>%
  rename(Year = yearID, `Batting AVG` = batting_avg) %>%
  arrange(desc(`Batting AVG`)) %>%
  slice(c(1:3, (n() - 2):n())) %>%
  gt() %>%
  tab_header(
    title = "Best / worst MLB seasons by AVG",
    subtitle = "Top / bottom are presented"
  )




