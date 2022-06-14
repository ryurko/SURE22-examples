# PURPOSE: Visualize Lahman batting data


# Load packages -----------------------------------------------------------

library(tidyverse)
library(Lahman)

# Create year summary -----------------------------------------------------

Batting <- as_tibble(Batting)

year_batting_summary <- Batting %>%
  filter(lgID %in% c("AL", "NL")) %>%
  group_by(yearID) %>%
  summarize_at(vars(HR, SO, BB, AB, H),
               sum, na.rm = TRUE) %>%
  mutate(batting_avg = H / AB)
  

# Construct basic ggplot examples -----------------------------------------

ggplot(data = year_batting_summary)

# or do this...
year_batting_summary %>%
  ggplot() +
  geom_point(mapping = aes(x = yearID, y = SO))

# ==
year_batting_summary %>%
  ggplot() +
  geom_point(aes(x = yearID, y = SO)) +
  geom_line(aes(x = yearID, y = SO))

# also ==
year_batting_summary %>%
  ggplot(aes(x = yearID, y = SO)) +
  geom_point(alpha = .75, size = .5) +
  geom_line() +
  scale_x_continuous(limits = c(2000, 2021)) +
  scale_y_continuous(breaks = seq(0, 45000, by = 15000))

# Change types of scalew
year_batting_summary %>%
  ggplot(aes(x = yearID, y = SO)) +
  geom_point(alpha = .75,
             aes(color = HR, size = BB)) +
  geom_line(color = "darkred", linetype = "dashed") +
  scale_color_gradient(low = "darkblue", high = "darkorange") +
  scale_size_continuous(breaks = seq(0, 15000, by = 2500)) +
  labs(x = "Year", y = "Strikeouts",
       color = "Homeruns", size = "Walks",
       title = "The rise of MLB's three true outcomes",
       caption = "Data courtesy of Lahman database") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15))
  
  #+
  #geom_smooth()
  
  
  