library(tidyverse)
library(janitor)
library(dplyr)
library(ggblend)
library(ggsoccer)
library(ggrepel)
library(viridis)

# wrangling
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")
View(wwc_shots)

shots <- wwc_shots |>
  mutate(goal = if_else(shot.outcome.name == "Goal", "Goal", "Miss"), 
         under_pressure = if_else(under_pressure == "TRUE", "Yes", "No")) |>
  clean_names()


# plot of shot locations by make/miss
ggplot(shots, aes(x = location_x, 
                  y = location_y, 
                  color = goal)) +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_point(size = 3, alpha = 0.6) +
  scale_color_manual(values = c("Goal" = "red", "Miss" = "gray70")) +
  coord_fixed(xlim = c(60, 120), 
              ylim = c(0, 80)) + 
  facet_wrap(~ goal) +
  theme_void() +
  labs(title = "Shot Locations on Attacking Half", color = "Outcome")


# without facet for make/miss
ggplot(data = shots, 
       aes(x = location_x, 
           y = location_y, 
           color = goal)) +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_point(size = 3, alpha = 0.5) +
  coord_fixed() +  # keeps aspect ratio square
  scale_color_manual(values = c("Goal" = "red", "Miss" = "gray70")) +
  labs(title = "Shot location by outcome",
       x = "",
       y = "",
       color = "Outcome") +
  theme_void()



# under pressure GOAL locations by play pattern 
goals_under_pressure <- shots |>
  filter(goal == "Goal", 
         under_pressure == "Yes")

ggplot(goals_under_pressure, aes(x = location_x, y = location_y, 
                                 color = play_pattern_name, 
                                 #size = avevelocity
                                 )) +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "#F8F8F8", 
                 colour = "#CCCCCC") +
  geom_point(size = 2, alpha = 0.7) +
  #geom_text_repel(label = player_name, size = 3, max.overlaps = 5) +
  scale_color_viridis_d(name = "Play Pattern") +
  labs(title = "Locations of 'under pressure' goals", 
       subtitle = "colored by play pattern") +
  theme_void(base_size = 14) +
  coord_fixed(xlim = c(60, 120), ylim = c(0, 80))



# under pressure shot locations by play pattern
under_pressure <- shots |>
  filter(under_pressure == "Yes")

ggplot(under_pressure, aes(x = location_x, y = location_y, 
                                 color = play_pattern_name)) +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "#F8F8F8", 
                 colour = "#CCCCCC") +
  geom_point(size = 2, alpha = 0.7) +
  #geom_text_repel(label = player_name, size = 3, max.overlaps = 5) +
  scale_color_viridis_d(name = "Play Pattern") +
  labs(title = "Locations of 'under pressure' shots", 
       subtitle = "colored by play pattern") +
  theme_void(base_size = 14) +
  coord_fixed(xlim = c(60, 120), ylim = c(0, 80))


# which goalies are most efficient/effective under pressure?
# which shooters are most clutch under pressure?
# making contact, deflecting shots 
# shots, filter for only on target shots using shot_outcome_name != "Off T", "Wayward"
# what proportion of these on target shots are being saved? deflected? 
# which goalies have the highest proportion of saved "on-target" shots?

  







# k-means clustering for all made shots 
shots_made_orig <- shots |> 
  filter(goal == "Goal") |> 
  select(location_x, location_y)

shots_scaled <- as.data.frame(scale(shots_made_orig))

first_kmeans <- kmeans(shots_scaled, 
                       centers = 4, 
                       nstart = 100)


shots_made_orig$cluster <- factor(first_kmeans$cluster)


ggplot(shots_made_orig, aes(x = location_x, y = location_y, 
                            color = cluster)) +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_point(size = 3, alpha = 0.7) +
  coord_fixed(xlim = c(60, 120), ylim = c(0, 80)) +  # right half of pitch
  ggthemes::scale_color_colorblind() +
  labs(title = "K-Means Clusters of Made Shots",
       color = "Cluster") +
  theme_void()



# k-means clustering for all made shots one-on-one with goalie

shots_one_on_one <- shots |>
  filter(goal == "Goal", shot_one_on_one == TRUE) |>
  select(location_x, location_y)

shots_scaled <- as.data.frame(scale(shots_one_on_one))

first_kmeans <- kmeans(shots_scaled, centers = 3, nstart = 100)

shots_one_on_one$cluster <- factor(first_kmeans$cluster)

ggplot(shots_one_on_one, aes(x = location_x, y = location_y, color = cluster)) +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_point(size = 3, alpha = 0.7) +
  coord_fixed(xlim = c(60, 120), ylim = c(0, 80)) +
  ggthemes::scale_color_colorblind() +
  labs(title = "K-Means Clusters of One-on-One Made Shots",
       color = "Cluster") +
  theme_void()




