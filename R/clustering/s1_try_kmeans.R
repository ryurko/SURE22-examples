# PURPOSE: K-means clustering of Gapminder data


# Load the data -----------------------------------------------------------

library(tidyverse)
library(dslabs)

gapminder <- as_tibble(gapminder)

gapminder %>%
  ggplot(aes(x = log(gdp))) +
  geom_histogram()


# Create clean data -----------------------------------------------------

clean_gapminder <- gapminder %>%
  filter(year == 2011, !is.na(gdp)) %>%
  mutate(log_gdp = log(gdp))

clean_gapminder %>%
  ggplot(aes(x = log_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.75) +
  theme_bw()


# Initial unscaled kmeans clustering --------------------------------------

init_kmeans <-
  kmeans(dplyr::select(clean_gapminder,
                       log_gdp, life_expectancy),
         algorithm = "Lloyd", centers = 3,
         nstart = 1)

clean_gapminder %>%
  mutate(country_clusters = 
           as.factor(init_kmeans$cluster)) %>%
  ggplot(aes(x = log_gdp, y = life_expectancy,
             color = country_clusters)) +
  geom_point(alpha = 0.75) +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_fixed()

clean_gapminder <- clean_gapminder %>%
  mutate(std_log_gdp = (log_gdp - mean(log_gdp)) / sd(log_gdp),
         std_life_exp = (life_expectancy - mean(life_expectancy)) / 
           sd(life_expectancy))

clean_gapminder <- clean_gapminder %>%
  mutate(std_log_gdp = as.numeric(scale(log_gdp)),
         std_life_exp = as.numeric(scale(life_expectancy)))

std_kmeans <-
  kmeans(dplyr::select(clean_gapminder,
                       std_log_gdp, std_life_exp),
         algorithm = "Hartigan-Wong", centers = 3,
         nstart = 30)

clean_gapminder %>%
  mutate(country_clusters = 
           as.factor(std_kmeans$cluster)) %>%
  ggplot(aes(x = log_gdp, y = life_expectancy,
             color = country_clusters)) +
  geom_point(alpha = 0.75) +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")



# K-means ++ --------------------------------------------------------------

library(flexclust)

init_kmeanspp <-
  kcca(dplyr::select(clean_gapminder,
                       std_log_gdp, std_life_exp),
       k = 3, control = list(initcent = "kmeanspp"))

clean_gapminder %>%
  mutate(country_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = log_gdp, y = life_expectancy,
             color = country_clusters)) +
  geom_point(alpha = 0.75) +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

