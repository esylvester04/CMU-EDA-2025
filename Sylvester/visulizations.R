library(tidyverse)
library(dplyr)
library(ggblend)
#library(ggsoccer)
library(ggrepel)
library(viridis)
library(scales)
library(plotly)

# wrangling
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")
View(wwc_shots)

shots <- wwc_shots |>
  mutate(goal = if_else(shot.outcome.name == "Goal", "Goal", "Miss"),
         under_pressure = if_else(is.na(under_pressure), 0, 1))|> 
           janitor::clean_names()



# IDEAS:
# which goalies are most efficient/effective under pressure?
# which shooters are most clutch under pressure?
# making contact, deflecting shots 
# shots, filter for only on target shots using shot_outcome_name != "Off T", "Wayward"
# what proportion of these on target shots are being saved? deflected? 
# which goalies have the highest proportion of saved "on-target" shots?





# Interactive version for Goalkeeper Performance on On-Target Shots:

on_target <- shots |>
  filter(!(shot_outcome_name %in% c("Off T", "Wayward")),
         !is.na(player_name_gk)) |>
  mutate(goal = if_else(shot_outcome_name == "Goal", "Goal", "Saved"))


on_target_summary <- on_target |>
  filter(shot_outcome_name %in% c("Goal", "Saved", "Saved Off T")) |>
  group_by(player_name_gk, shot_outcome_name) |>
  count() |>
  group_by(player_name_gk) |>
  mutate(total = sum(n),
         prop = n / total) |>
  filter(total >= 10) |>
  ungroup()


team_lookup <- on_target |>
  group_by(player_name_gk) |>
  summarize(possession_team_name = first(possession_team_name), .groups = "drop")

# join teams info back in
on_target_summary <- on_target_summary |>
  left_join(team_lookup, by = "player_name_gk")


# goalkeepers in order
save_order <- on_target_summary |>
  filter(shot_outcome_name %in% c("Saved", "Saved Off T")) |>
  group_by(player_name_gk) |>
  summarize(prop_saved = sum(prop)) |>
  arrange(desc(prop_saved)) |>
  pull(player_name_gk)

outcome_colors <- c(
  "Saved" = "forestgreen",
  "Saved Off T" = "gold",
  "Goal" = "firebrick"
)


g <- ggplot(on_target_summary,
            aes(x = factor(player_name_gk, levels = rev(save_order)),
                y = prop,
                fill = shot_outcome_name,
                text = paste0("Goalkeeper: ", player_name_gk,
                              "<br>Team: ", possession_team_name, 
                              "<br>Outcome: ", shot_outcome_name,
                              "<br>Proportion: ", scales::percent(prop, accuracy = 0.1)))) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_fill_manual(values = outcome_colors, drop = FALSE) +
  labs(
    title = "Goalkeeper Performance on On-Target Shots",
    subtitle = "Keepers who faced at least 10 shots on goal; ordered by total saved %",
    x = "Goalkeeper", y = "Proportion",
    fill = "Shot Outcome"
  ) +
  theme_minimal(base_size = 13)

# making interactive
# could potentially add team names or logos / pictures

ggplotly(g, tooltip = "text") %>%
  layout(
    title = list(
      text = paste0(
        "Goalkeeper Performance on On-Target Shots",
        "<br><sup>Keepers who faced at least 10 shots on goal; ordered by total saved %</sup>"
      )
    )
  )

#downloading the HTML:
p <- ggplotly(g, tooltip = "text")
htmlwidgets::saveWidget(p, "interactive_plot.html")







# Pitch graphics:
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


# under pressure GOAL locations by play pattern 
goals_under_pressure <- shots |>
  filter(goal == "Goal", 
         under_pressure == 1)

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





# Some Clustering attempts 

cluster_data_small <- shots |>
  select(dist_to_goal, angle_to_goal, under_pressure) |>
  drop_na()

scaled_data_small <- scale(cluster_data_small)

set.seed(42)
kmod_small <- kmeans(scaled_data_small, centers = 3, nstart = 25)

cluster_data_small <- cluster_data_small |>
  mutate(cluster = factor(kmod_small$cluster))


factoextra::fviz_cluster(kmod_small, data = scaled_data_small,
             geom = "point",
             ellipse.type = "norm",
             main = "K-Means Clustering with Fewer Variables")


# Heat Map:
shots |>
  ggplot(aes(x = location_x, y = location_y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", contour = TRUE) +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Heatmap of Shot Locations") +
  theme_minimal()



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




