# one_shot_wonders_clean.R
# libraries
library(tidyverse)
library(ggrepel)
library(ggsoccer)
library(gridExtra)
library(viridis)

# 1) per-player counts
player_stats <- wwc_shots %>%
  group_by(player.name) %>%
  summarise(
    shots      = n(),
    goals      = sum(shot.outcome.name == "Goal", na.rm = TRUE),
    conversion = goals / shots,
    .groups    = "drop"
  )

# 2) grab only the one-shot wonders (1 shot, 1 goal) with their goal details
one_shot <- wwc_shots %>%
  semi_join(player_stats %>% filter(shots == 1, goals == 1), by = "player.name") %>%
  filter(shot.outcome.name == "Goal") %>%
  mutate(time_of_goal = minute + second/60)

# 3) horizontal bar of when they scored
p1 <- one_shot %>%
  arrange(time_of_goal) %>%
  mutate(player.name = factor(player.name, levels = unique(player.name))) %>%
  ggplot(aes(time_of_goal, player.name, fill = play_pattern.name)) +
  geom_col() +
  scale_fill_viridis_d(name = "Play Pattern") +
  labs(
    title = "When One-Shot Wonders Scored",
    x     = "Match Minute",
    y     = NULL
  ) +
  theme_minimal(base_size = 14)

# 4) pitch map of their lone-shot goals
p2 <- ggplot(one_shot, aes(x = location.x, y = location.y, color = play_pattern.name)) +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#F8F8F8", colour = "#CCCCCC") +
  geom_point(size = 5) +
  geom_text_repel(aes(label = player.name),
                  size = 3, max.overlaps = 5) +
  scale_color_viridis_d(name = "Play Pattern") +
  labs(title = "Goal Locations for One-Shot Wonders") +
  theme_void(base_size = 14)

# 5) scatter all goals by distance vs. angle, highlight one-shot wonders
all_goals <- wwc_shots %>%
  filter(shot.outcome.name == "Goal")

p3 <- ggplot() +
  # all goals in light grey
  geom_point(data = all_goals,
             aes(x = DistToGoal, y = AngleToGoal),
             color = "grey80", alpha = 0.5) +
  # one-shot wonders in color
  geom_point(data = one_shot,
             aes(x = DistToGoal, y = AngleToGoal, color = player.name),
             size = 3) +
  geom_text_repel(data = one_shot,
                  aes(x = DistToGoal, y = AngleToGoal, label = player.name),
                  size = 3, nudge_y = 2) +
  scale_color_viridis_d(name = "Player") +
  labs(
    title = "Where One-Shot Wonders Scored vs. All Goals",
    x     = "Distance to Goal (m)",
    y     = "Shot Angle to Goal (°)"
  ) +
  theme_minimal(base_size = 14)

# 6) stitch the three panels back together
grid.arrange(p1, p2, p3, nrow = 3)
