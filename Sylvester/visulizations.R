library(tidyverse)
library(dplyr)
library(ggblend)
library(ggsoccer)
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

shots <- shots |>
  mutate(player_name_gk = case_when(
    player_name_gk == "Olivia Alexandra Davies Isip McDaniel" ~ "Olivia McDaniel",
    player_name_gk == "Yenith Elizabett Bailey de la Cruz" ~ "Yenith Bailey",
    TRUE ~ player_name_gk
  ))



# Density of shot attempts plot
library(ggtext)
shots$goal <- fct_relevel(shots$goal, "Miss", "Goal")

ggplot(shots, aes(x = location_x, y = location_y)) +
  annotate_pitch(dimensions = pitch_statsbomb,
                 fill = "#F8F8F8",
                 colour = "#AAAAAA")+
  geom_density_2d_filled(alpha = 0.7, contour_var = "ndensity") +
  coord_fixed(xlim = c(80, 125), ylim = c(15, 65)) +
  ggh4x::facet_wrap2(~ goal, strip.position = "bottom") +
  theme_void(base_size = 13) +
  labs(
    title = "Density of Shot Attempts by Outcome",
    subtitle = "Normalized density estimate of shot locations by outcome (goal vs miss)", 
    fill = "Shot Concentration"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    plot.subtitle = element_text(
      size = 14,
      face = "italic",
      hjust = 0.5,
      margin = margin(t = 10, b = 15)
    ),
    strip.text = element_text(size = 16, face = "bold"),
    strip.placement = "outside",
    plot.margin = margin(t = 30, r = 20, b = 40, l = 20),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_fill_viridis_d(option = "magma", name = "Shot Density")
















on_target <- shots |>
  filter(!(shot_outcome_name %in% c("Off T", "Wayward")),
         !is.na(player_name_gk)) |>
  mutate(goal = if_else(shot_outcome_name == "Goal", "Goal", "Saved"))

goals_under_pressure <- shots |>
  filter(goal == "Goal", 
         under_pressure == 1)


# IDEAS:
# which goalies are most efficient/effective under pressure?
# which shooters are most clutch under pressure?
# making contact, deflecting shots 
# shots, filter for only on target shots using shot_outcome_name != "Off T", "Wayward"
# what proportion of these on target shots are being saved? deflected? 
# which goalies have the highest proportion of saved "on-target" shots?



# Interactive plotly for Goalkeeper performance for on target shots:

on_target_summary <- on_target |>
  filter(shot_outcome_name %in% c("Goal", "Saved", "Saved Off T")) |>
  group_by(player_name_gk, shot_outcome_name) |>
  count() |>
  group_by(player_name_gk) |>
  mutate(total = sum(n),
         prop = n / total) |>
  filter(total >= 12) |>
  ungroup()


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
                              "<br>Outcome: ", shot_outcome_name,
                              "<br>Proportion: ", scales::percent(prop, accuracy = 0.1)))) +
  geom_col(width = 0.6)  +
  coord_flip() +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_fill_manual(values = outcome_colors, drop = FALSE) +
  labs(
    title = "Goalkeeper Performance on On-Target Shots",
    subtitle = "Keepers who faced at least 12 shots on goal; ordered by total saved %",
    x = "Goalkeeper", y = "Proportion",
    fill = "Shot Outcome"
  ) +
  theme_minimal(base_size = 13)


# making interactive
# could potentially add team names or logos / pictures

ggplotly(g, tooltip = "text") %>%
  layout(
    font = list(family = "Arial", size = 12),
    title = list(
      text = paste0(
        "Save Rates for Goalkeepers Facing On-Target Shots 
        <sup>Includes only goalkeepers with 10+ on-target shots faced</sup>"
      ),
      xanchor = "center",
      x = 0.5
    # ),
    # legend = list(orientation = "h", x = 0.3, y = -0.15),
    # margin = list(l = 100, r = 20, b = 60, t = 80)
  ))

# downloading the HTML:
p <- ggplotly(g, tooltip = "text")
htmlwidgets::saveWidget(p, "interactive.html")







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



shots_in_box <- shots %>%
  filter(location_x >= 102, location_x <= 120,
         location_y >= 18, location_y <= 62)

ggplot(shots_in_box, aes(x = location_x, 
                         y = location_y, 
                         color = goal)) +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_point(size = 3, alpha = 0.6) +
  scale_color_manual(values = c("Goal" = "red", "Miss" = "gray70")) +
  coord_fixed(xlim = c(100, 122), ylim = c(10, 70)) + 
  facet_wrap(~ goal) +
  theme_void() +
  labs(title = "Shot Locations Inside the Box", color = "Outcome")



# Heatmap of shot locations
  
 ggplot(shots, aes(x = location_x, y = location_y)) +
     annotate_pitch(dimensions = pitch_statsbomb,
                    fill = "#F8F8F8", colour = "#CCCCCC") +
     geom_density_2d_filled(alpha = 0.8, contour_var = "ndensity") +
     coord_fixed(xlim = c(80, 125), ylim = c(15, 65)) +
     facet_wrap(~ goal) +
     theme_void(base_size = 13) +
     labs(title = "Shot Density Inside the Box", fill = "Density") +
     scale_fill_viridis_d(option = "cividis", name = "Shot Density")
 
 
 library(forcats)
 
 shots$goal <- fct_relevel(shots$goal, "Miss", "Goal")
 
 
 ggplot(shots, aes(x = location_x, y = location_y)) +
   annotate_pitch(dimensions = pitch_statsbomb,
                  fill = "#F8F8F8", colour = "#AAAAAA") +
   geom_density_2d_filled(alpha = 0.8, contour_var = "ndensity") +
   coord_fixed(xlim = c(80, 125), ylim = c(15, 65)) +
   facet_wrap(~ goal) +
   theme_void(base_size = 13) +
   labs(
     title = "Density of Shot Attempts by Outcome",
     fill = "Shot Concentration",
     caption = "Normalized density estimate of shot locations\nby outcome (goal vs miss)")+

   #theme(
    #plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
   theme_void(base_size = 13) +
   theme(
     plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
     strip.text = element_text(size = 16, face = "bold"),
     legend.title = element_text(size = 12),
     legend.text = element_text(size = 10),
     plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 10))
   ) +
   scale_fill_viridis_d(option = "turbo", name = "Shot Concentration")
 
 
 
 
 
 
 

 # under pressure GOAL locations by play pattern 
goals_under_pressure <- shots |>
  filter(goal == "Goal", 
         under_pressure == 1)

ggplot(goals_under_pressure, aes(x = location_x, y = location_y, 
                                 color = position_name, 
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



#heatmap of under pressure shot locations 

ggplot(shots, aes(x = location_x, 
                                 y = location_y)) +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "#F8F8F8", 
                 colour = "#CCCCCC"
                 ) +
  geom_hex(binwidth = c(1,1)) +
  labs(title = "Locations of 'under pressure' shots"
       ) +
  theme_void(base_size = 14) +
  coord_fixed(xlim = c(60, 120), ylim = c(0, 80)) 



ggplot(shots, aes(x = shot_end_location_x, 
                  y = shot_end_location_y)) +
  geom_rect(aes(xmin = 100, xmax = 120, ymin = 18, ymax = 62),
            fill = NA, color = "black", size = 1) +
  geom_hex(binwidth = c(1, 1)) +
  labs(title = "Locations of Shot Ends Around Goal") +
  theme_void(base_size = 14) +
  coord_fixed()



# Some Clustering attempts 

# hex bin clustering of under pressure shots
hex_summary <- shots |>
  filter(under_pressure == TRUE) |>
  mutate(hex_x = floor(location_x),
         hex_y = floor(location_y)) |>
  group_by(hex_x, hex_y) |>
  summarize(n_shots = n(), 
            avg_distance = mean(dist_to_goal, na.rm = TRUE),
            .groups = "drop")

hex_scaled <- scale(hex_summary[, c("n_shots", "avg_distance")])
hex_clusters <- kmeans(hex_scaled, centers = 3, nstart = 25)
hex_summary$cluster <- factor(hex_clusters$cluster)

ggplot(hex_summary, aes(x = hex_x, y = hex_y, fill = cluster)) +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_tile(color = "white", size = 0.1) +
  coord_fixed(xlim = c(60, 120), ylim = c(0, 80)) +
  ggthemes::scale_color_colorblind() +
  labs(title = "Hex Bin Clustering of 'Under Pressure' Shots",
       fill = "Cluster") +
  theme_void()





# More clustering

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



# k-means clustering for all made shots 
shots_made_orig <- shots |> 
  filter(goal == "Goal") |> 
  select(location_x, location_y)

shots_scaled <- as.data.frame(scale(shots_made_orig))

first_kmeans <- kmeans(shots_scaled, 
                       centers = 3, 
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

centroids <- as.data.frame(first_kmeans$centers)
centroids$cluster <- factor(1:3)

ggplot(shots_made_orig, aes(x = location_x, y = location_y, color = cluster)) +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_point(alpha = 0.7) +
  geom_point(data = centroids, aes(x = location_x, y = location_y), 
             color = "black", shape = 4, size = 5, stroke = 2) +
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






ggplot(on_target, aes(x = location_x, y = location_y)) +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_segment(aes(xend = shot_end_location_x, 
                   yend = shot_end_location_y), 
               arrow = arrow(length = unit(0.1, "inches")), 
               alpha = 0.7, color = "darkred") +
  coord_fixed(xlim = c(60, 120), ylim = c(0, 80)) +
  labs(title = "Shot Directions (Start to End Location)") +
  theme_void()

