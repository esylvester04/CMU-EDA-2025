library(tidyverse)
library(janitor)
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")


shots <- wwc_shots |>
  mutate(goal = if_else(shot.outcome.name == "Goal", "Goal", "Miss"))

shots <- shots |>
  clean_names()


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
  theme_linedraw()

