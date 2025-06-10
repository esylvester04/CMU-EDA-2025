library(tidyverse)
library(janitor)
library(dplyr)
library(ggblend)


# wrangling
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")
View(wwc_shots)

shots <- wwc_shots |>
  mutate(goal = if_else(shot.outcome.name == "Goal", "Goal", "Miss"))

shots <- shots |>
  clean_names()


# Starting Visualizations:
# bar chart for proportion of shots by shot outcome 
shots |>
  count(shot_outcome_name) |>
  mutate(prop = n/sum(n),
         shot_outcome_name = fct_reorder(shot_outcome_name, n)) |>
  ggplot(aes(x = shot_outcome_name, y = prop)) +
  geom_col(fill = "navy") +
  labs(title = "Proportion of shot by outcome",
       y = "Proportion",
       x = "Shot outcome") +
  coord_flip() +
  theme_minimal() 



# plot of shot locations by make/miss
ggplot(data = shots, 
       aes(x = location_x, 
           y = location_y, 
           color = goal)) +
  geom_point(alpha = 0.5, size = 2) +
  coord_fixed() +  # keeps aspect ratio square
  scale_color_manual(values = c("Miss" = "gray70", "Goal" = "red")) +
  labs(title = "Shot location by outcome",
       x = "",
       y = "",
       color = "Outcome") +
  facet_wrap(~ goal) +
  theme_void()


#without facet 
ggplot(data = shots, 
       aes(x = location_x, 
           y = location_y, 
           color = goal)) +
  geom_point(size = 3, alpha = 0.5) +
  coord_fixed() +  # keeps aspect ratio square
  scale_color_manual(values = c("Goal" = "red", "Miss" = "gray70")) +
  labs(title = "Shot location by outcome",
       x = "",
       y = "",
       color = "Outcome") +
  theme_void()


# k-means clustering for all made shots 
shots_made <- shots |>
  filter(goal == "Goal") |>
  select(location_x, 
         location_y) |>
  scale() |>
  as.data.frame()

first_kmeans <- shots_made |>
  kmeans(centers = 3, nstart = 30)  

shots_made |>
  mutate(cluster = factor(first_kmeans$cluster)) |>
  ggplot(aes(x = location_x, 
             y = location_y, 
             color = cluster)) +
  geom_point(size = 2) +
  ggthemes::scale_color_colorblind()

