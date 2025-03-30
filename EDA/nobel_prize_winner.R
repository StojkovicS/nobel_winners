# libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)

# data
nobel <- read_csv("nobel.csv")
head(nobel)

table(nobel$sex)
#top_gender <- "male"

nobel %>%
  group_by(birth_country) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

#top_country <- "United States of America"

prop_usa_winners <- nobel %>%
  mutate(is_us = ifelse(birth_country == "United States of America", 1, 0)) %>%
  mutate(decade=floor(year/10)*10) %>% 
  group_by(decade) %>% 
  summarize(proportion_us_born=mean(is_us, na.rm=T)) %>% 
  arrange(desc(proportion_us_born))


nobel %>%
  mutate(is_uk = ifelse(birth_country == "Germany", 1, 0)) %>%
  mutate(decade=floor(year/10)*10) %>% 
  group_by(decade) %>% 
  summarize(proportion_uk_born=mean(is_uk, na.rm=T)) %>% 
  arrange(desc(proportion_uk_born))


#max_decade_usa <- 2000

# prop usa germany france
prop_countries_winners <- nobel %>%
  filter(birth_country %in% c("United States of America", "United Kingdom", "Germany", "France")) %>%
  mutate(is_usa = ifelse(birth_country == "United States of America", 1, 0),
         is_uk = ifelse(birth_country == "United Kingdom", 1, 0),
         is_germany = ifelse(birth_country == "Germany", 1, 0),
         is_france = ifelse(birth_country == "France", 1, 0)) %>%
  mutate(decade = floor(year / 10) * 10) %>% 
  group_by(decade) %>% 
  summarize(proportion_usa_born = mean(is_usa, na.rm = TRUE),
            proportion_uk_born = mean(is_uk, na.rm = TRUE),
            proportion_germany_born = mean(is_germany, na.rm = TRUE),
            proportion_france_born = mean(is_france, na.rm = TRUE)) %>% 
  arrange(desc(decade))


# plot 
ggplot(prop_usa_winners, aes(decade, proportion_us_born)) +
  geom_line() + geom_point() + # here is how you get line plot with dots
  scale_y_continuous(labels = scales::percent, limits = 0:1, expand = c(0,0))

# four countries
ggplot(prop_countries_winners, aes(decade)) +
  geom_line(aes(y = proportion_usa_born, color = "USA"), size = 1) +
  geom_point(aes(y = proportion_usa_born, color = "USA"), size = 2) +
  geom_line(aes(y = proportion_uk_born, color = "UK"), size = 1) +
  geom_point(aes(y = proportion_uk_born, color = "UK"), size = 2) +
  geom_line(aes(y = proportion_germany_born, color = "Germany"), size = 1) +
  geom_point(aes(y = proportion_germany_born, color = "Germany"), size = 2) +
  geom_line(aes(y = proportion_france_born, color = "France"), size = 1) +
  geom_point(aes(y = proportion_france_born, color = "France"), size = 2) +
  scale_color_manual(values = c("USA" = "#1f77b4", "UK" = "#ff7f0e", "Germany" = "#2ca02c", "France" = "#d62728"),
                     breaks = c("USA", "UK", "Germany", "France")) +
#  scale_linetype_manual(values = c("UK" = "dotted")) +
  labs(title = "Proportion of Nobel Prize Winners by Decade and Country of Birth",
       x = "Decade",
       y = "Proportion Born",
       color = "Country") +
  scale_y_continuous(labels = scales::percent, limits = 0:1, expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
    theme_minimal() +
  guides(linetype = FALSE)



prop_female_winners <- nobel %>%
  mutate(female_winner = sex == "Female",
         decade = floor(year / 10) * 10) %>%
  select(decade, category, female_winner)  %>% 
  group_by(decade, category) %>%
  summarize(proportion = mean(female_winner))
prop_female_winners

max_female_list <- list(year = 1990, category = "Literature")
max_female_list

nobel %>% 
  filter(sex=="Female") %>% 
  arrange(year)

first_woman_name <- "Marie Curie, née Sklodowska"

first_woman_category <- "Physics"

repeats <- nobel  %>% 
  count(full_name) %>% 
  filter(n>1) %>% 
  select(full_name)

repeats <- as.list(repeats)
repeats