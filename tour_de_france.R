



## call package
library(tidyverse)
library(paletteer)
library(ggthemes)
library(ggrepel)

# tidytuesday data
tour_de_france <-
  read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv'
  )
glimpse(tour_de_france)
View(tour_de_france)



# remove na from the time_overall
tdf <- tour_de_france %>% filter(!is.na(time_overall)) %>%
  mutate(time_overall = round(time_overall))
tdf


#countries with most number of wins
tdf2 <- tour_de_france %>% select(winner_team, nationality) %>%
  count(nationality, sort = TRUE) %>% view()
tdf2

# plot
ggplot(tdf, aes(x = time_overall / 24, distance, colour = stage_wins)) +
  geom_point() +
  facet_wrap( ~ nationality) +
  expand_limits(x = 2) +
  theme_solarized() +
  scale_x_continuous(name = "Number of Days taken to complete the Distance and win the race \n
                     (converted hours into day)") +
  scale_y_continuous(name = "Distance") +
  scale_colour_paletteer_c("harrypotter::harrypotter") +
  labs(title = "Days(hours) against the Distance to win the Race and lift the Title",
       caption = "TidyTuesday: tour de france")


ggplot(tdf2, aes(n, fct_reorder(nationality, n))) +
  geom_point() +
  geom_text(aes(label = n), hjust = -0.5, check_overlap = TRUE) +
  xlab(label = "Number of wins by Country") +
  ylab(label = "Country") +
  theme_economist() +
  scale_colour_economist() +
  labs(title = "Most number of Wins by Country wise",
       caption = "TidyTuesday")



library(broom)
linear <- lm(time_margin ~ distance + time_overall, data = tdf)
tidy(linear)

# whether distance has any impact on time_margin or not.
theme_set(theme_clean())
ggplot(tdf, aes(distance, time_margin, colour = time_overall)) +
  geom_point() +
  stat_smooth(span = 1, method = "lm")
