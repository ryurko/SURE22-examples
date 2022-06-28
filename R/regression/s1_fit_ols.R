# PURPOSE: Initial regression models of life expectancy


# Load data ---------------------------------------------------------------

library(tidyverse)
library(dslabs)
gapminder <- as_tibble(gapminder)
clean_gapminder <- gapminder %>%
  filter(year == 2011, !is.na(gdp)) %>%
  mutate(log_gdp = log(gdp))
clean_gapminder

clean_gapminder %>%
  ggplot(aes(x = life_expectancy)) +
  geom_histogram() +
  theme_bw()


# Model relationship between gdp and life expectancy ----------------------

# View plots of relationship

hist(clean_gapminder$gdp)

gdp_plot <- clean_gapminder %>%
  ggplot(aes(x = log_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  theme_bw()

init_lm <- lm(life_expectancy ~ log_gdp, data = clean_gapminder)
summary(init_lm)

no_int_lm <- lm(life_expectancy ~ 0 + log_gdp, data = clean_gapminder)


gdp_plot <- clean_gapminder %>%
  ggplot(aes(x = log_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              formula = "y ~ 0 + x") +
  theme_bw()

# Play around with predictions --------------------------------------------

train_preds <- predict(init_lm)
head(train_preds)
head(init_lm$fitted.values)

us_data <- clean_gapminder %>%
  filter(country == 'United States')

new_us_data <- us_data %>%
  dplyr::select(country, gdp) %>%
  slice(rep(1, 3)) %>%
  mutate(adj_factor = c(.0001, .21, .75),
         log_gdp = log(gdp * adj_factor))
new_us_data$pred_life_exp <-
  predict(init_lm, newdata = new_us_data)

gdp_plot +
  geom_point(data = new_us_data,
             aes(x = log_gdp, y = pred_life_exp),
             color = "red", size = 6,
             alpha = .8)

gdp_plot +
  #geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = init_lm$coefficients[2],
              intercept = init_lm$coefficients[1],
              color = "red")


clean_gapminder %>%
  mutate(pred_vals = init_lm$fitted.values) %>%
  ggplot(aes(x = pred_vals, y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red",
              size = 2) +
  theme_bw()

clean_gapminder <- 
  broom::augment(init_lm, clean_gapminder)

clean_gapminder %>%
  ggplot(aes(x = .fitted, y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red",
              size = 2) +
  theme_bw()


clean_gapminder %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "darkred") +
  geom_smooth(se = FALSE) +
  theme_bw()



# Multiple linear regression ----------------------------------------------

multi_lm <- lm(life_expectancy ~ log_gdp + fertility, data = clean_gapminder)
summary(multi_lm)







