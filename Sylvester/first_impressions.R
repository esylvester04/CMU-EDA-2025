library(tidyverse)
library(janitor)
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")
View(wwc_shots)

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


# proportion of shots by play pattern
shots |>
  count(play_pattern_name) |>
  mutate(prop = n/sum(n),
         play_pattern_name = fct_reorder(play_pattern_name, n)) |>
  ggplot(aes(x = play_pattern_name, y = prop)) +
  geom_col(fill = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Proportion of shot by play pattern",
       y = "Proportion",
       x = "Play pattern")


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



library(grid) 
library(jpeg)

pitch_image <- readJPEG("pitch.jpg")  
pitch_grob <- rasterGrob(pitch_image, width = unit(1, "npc"), height = unit(1, "npc"))

ggplot(data = shots, aes(x = location_x, 
                         y = location_y, 
                         color = goal)) +
  annotation_custom(pitch_grob, 
                    xmin = 0, xmax = 105, 
                    ymin = 0, ymax = 68) +
  geom_point(alpha = 0.6, size = 2) +
  coord_fixed(ratio = 68 / 105, 
              xlim = c(0, 105), 
              ylim = c(0, 68)) +
  scale_color_manual(values = c("Miss" = "gray70", 
                                "Goal" = "red")) +
  facet_wrap(~ goal) +
  labs(title = "Shot Location by Outcome", 
       color = "Outcome") +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )






shots |>
  count(play_pattern_name) |>
  mutate(prop = n/sum(n),
         play_pattern_name = fct_reorder(play_pattern_name, n)) |>
  ggplot(aes(x = play_pattern_name, y = prop)) +
  geom_col(fill = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Proportion of shot by play pattern",
       y = "Proportion",
       x = "Play pattern")


shots |>
  count(play_pattern_name, goal) |>
  group_by(play_pattern_name) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  mutate(play_pattern_name = fct_reorder(play_pattern_name, -n)) |>
  ggplot(aes(x = play_pattern_name, y = prop, fill = goal)) +
  geom_col(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Proportion of shots by play pattern and outcome",
    y = "Proportion (within play pattern)",
    x = "Play pattern",
    fill = "Outcome"
  )





# Shot distance from goal over time of the game, color by make/miss
ggplot(data = shots, 
       aes(x = minute, y = dist_to_goal)) +
  geom_point(aes(color = goal), alpha = 0.5) +
  labs(title = "Shot distance over time",
       x = "Minute of game",
       y = "Distance to goal (m)",
       color = "Make") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_color_manual(values = c("Miss" = "gray", "Goal" = "darkgreen")) +
  scale_alpha_manual(values = c("Goal" = 0.8))


library(ggplot2)
library(dplyr)
library(forcats)
library(plotly)

# Base ggplot
p <- shots |>
  ggplot(aes(x = minute, y = dist_to_goal,
             color = goal,
             text = paste("Team:", possession_team_name,
                          "<br>Outcome:", goal,
                          "<br>Minute:", minute,
                          "<br>Distance:", dist_to_goal))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Shot Distance Over Time",
    x = "Minute of Game",
    y = "Distance to Goal (m)",
    color = "Outcome"
  )

# Make it interactive
ggplotly(p, tooltip = "text")





ggplot(data = shots, 
       aes(x = dist_to_goal, y = avevelocity)) +
  geom_point(aes(color = as.factor(period)), alpha = 0.6) +
  labs(
    title = "Shot velocity over time by position",
    x = "Minute of game",
    y = "Average shot veclocity (m/s)",
    color = "Period") +
  theme_minimal() 



#shot outcome by proximity to defender
shots |>
  ggplot(aes(x = distance_to_d1_360, fill = shot_outcome_name)) +
  geom_density(alpha = 0.6) +
  labs(x = "Distance to closest defender (m)", 
       title = "Shot outcome by proximity to defender") +
  theme_minimal()


ggplot(data = shots, 
       aes(x = minute, y = dist_to_goal)) +
  geom_point(aes(color = position_name), alpha = 0.6) +
  labs(
    title = "Shot distance over time by position",
    x = "Minute of game",
    y = "Distance to G=goal (m)",
    color = "Position") +
  theme_minimal()

shots |>
  ggplot(aes(x = angle_to_goal, y = dist_to_goal, color = shot_outcome_name)) +
  geom_point(alpha = 0.6) +
  labs(x = "Angle to Goal", y = "Distance to Goal (m)", color = "Outcome",
       title = "Shot Outcome by Angle and Distance") +
  theme_minimal()




ggplot(data = shots, 
       aes(x = minute, 
           y = dist_to_goal)) +
  geom_point(aes(color = position_name), alpha = 0.6) +
  theme_minimal()

ggplot(data = shots, 
       aes(x = minute, 
           y = dist_to_goal)) +
  geom_point(aes(color = shot_technique_name), alpha = 0.6) +
  theme_minimal()


ggplot(data = shots, 
       aes(x = minute, y = dist_to_goal)) +
  geom_point(aes(color = position_name), alpha = 0.6) +
  geom_smooth(aes(color = position_name), se = FALSE) + #trend lines are confusing 
  labs(
    title = "Shot distance over time by position",
    x = "Minute of game",
    y = "Distance to G=goal (m)",
    color = "Position") +
  theme_minimal()



ggplot(shots, 
       aes(x = minute, 
           y = angle_deviation)) +
  geom_point(aes(color = shot_technique_name), alpha = 0.5) +
  theme_minimal()


library(ggridges)

shots |> 
  ggplot(aes(x = avevelocity, y = shot_technique_name)) +
  geom_density_ridges(scale = 1) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(
    x = "Average shot velocity (m/s)",
    y = "Shot technique",
    title = "Distribution of shot velocities by technique") +
  theme_minimal()

shots |>
  ggplot(aes(x = location_x, y = location_y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", contour = TRUE) +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Heatmap of Shot Locations") +
  theme_minimal()



top_shooters <- shots |>
  count(player_name, sort = TRUE) |>
  slice_head(n = 10) |> 
  pull(player_name)

filtered_shots <- shots |>
  filter(player_name %in% top_shooters)

filtered_shots <- filtered_shots |> 
  mutate(last_name = word(player_name, -1)) 

ggplot(data = filtered_shots, 
       aes(x = location_x, 
           y = location_y, 
           color = goal)) +
  geom_point(alpha = 0.5, size = 2) +
  coord_fixed() +
  scale_color_manual(values = c("Miss" = "gray70", "Goal" = "red")) +
  labs(title = "Shot Location by Outcome",
       x = "",
       y = "",
       color = "Outcome") +
  facet_wrap(~ last_name) + 
  theme_minimal()


