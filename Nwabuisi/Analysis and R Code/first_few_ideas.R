# team_analysis.R

# I load the libraries I need for data manipulation and fancy labels
library(tidyverse)
library(ggrepel)

if (!exists("wwc_shots")) {
  wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")
}
# I calculate each team’s average shot distance, conversion rate, and shot count
team_stats <- wwc_shots %>%
  group_by(possession_team.name) %>%
  summarise(
    avg_dist   = mean(DistToGoal, na.rm = TRUE),
    conv_rate  = mean(shot.outcome.name == "Goal", na.rm = TRUE),
    shot_count = n(),
    .groups    = "drop"
  )

# I plot a bubble chart showing how average distance relates to goal conversion
ggplot(team_stats, aes(avg_dist, conv_rate, size = shot_count)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey50") +  # trend line
  geom_point(alpha = 0.7, color = "#2c7fb8") +                                     # team bubbles
  geom_text_repel(aes(label = possession_team.name), size = 3, max.overlaps = 15) + # team labels
  scale_size_continuous(range = c(3, 12)) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  labs(
    title    = "Team Avg Distance vs. Goal Conversion",
    subtitle = "Bubble size = total shots",
    x        = "Avg Distance (m)",
    y        = "Conversion Rate",
    size     = "Shots"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )

# I filter for open-play and counterattacks, then bin possession time into categories
wwc_poss <- wwc_shots %>%
  filter(
    play_pattern.name %in% c("Open Play","From Counter"),
    !is.na(TimeInPoss),
    !is.na(shot.outcome.name)
  ) %>%
  mutate(
    poss_bin = cut(
      TimeInPoss,
      breaks = c(0, 3, 10, 20, Inf),
      labels = c("0–3s (Fast)", "3–10s (Normal)", "10–20s (Build-up)", ">20s (Slow)"),
      right  = FALSE
    ),
    is_goal = shot.outcome.name == "Goal"
  )

# I summarize conversion rate and number of attempts by play pattern and possession bin
summary_poss <- wwc_poss %>%
  group_by(play_pattern.name, poss_bin) %>%
  summarise(
    conversion_rate = mean(is_goal, na.rm = TRUE),
    attempts        = n(),
    .groups         = "drop"
  )

# I draw a line-and-point plot comparing conversion across possession bins
ggplot(summary_poss, aes(poss_bin, conversion_rate, color = play_pattern.name, group = play_pattern.name)) +
  geom_line(size = 1.2) +
  geom_point(aes(size = attempts), alpha = 0.8) +
  scale_color_manual(values = c("Open Play" = "#1f78b4", "From Counter" = "#e31a1c")) +
  scale_y_continuous(labels = scales::percent_format(1), limits = c(0, NA)) +
  scale_size(range = c(3, 8)) +
  labs(
    title    = "Conversion by Possession Duration",
    subtitle = "Open play vs. counter",
    x        = "Time in Possession",
    y        = "Conversion Rate",
    color    = "Pattern",
    size     = "Attempts"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.title      = element_text(face = "bold")
  )

# tempo_analysis.R

# I load the package I need for beeswarm plots
library(ggbeeswarm)

# I categorize shots by preparation time: instant, normal, or delayed
wwc_tempo <- wwc_shots %>%
  filter(!is.na(duration), !is.na(shot.outcome.name)) %>%
  mutate(
    time_category = case_when(
      duration < 0.3 ~ "Instant (<0.3s)",
      duration > 1.0 ~ "Delayed (>1.0s)",
      TRUE           ~ "Normal (0.3–1.0s)"
    ) %>% factor(levels = c("Instant (<0.3s)", "Normal (0.3–1.0s)", "Delayed (>1.0s)")),
    is_goal = factor(if_else(shot.outcome.name == "Goal", "Goal", "No Goal"),
                     levels = c("No Goal", "Goal"))
  )

# I summarize how conversion rate varies across those time categories
tempo_summary <- wwc_tempo %>%
  group_by(time_category) %>%
  summarise(
    attempts  = n(),
    goals     = sum(is_goal == "Goal"),
    conv_rate = goals / attempts,
    .groups   = "drop"
  )

# I plot conversion rate as a bar chart for the prep-time categories
ggplot(tempo_summary, aes(time_category, conv_rate, fill = time_category)) +
  geom_col(color = "white", width = 0.7) +
  geom_text(aes(label = scales::percent(conv_rate, 1)), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format(1), expand = c(0, 0.02)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title    = "Conversion by Shot Prep Time",
    subtitle = "Instant vs. normal vs. delayed",
    x        = "Prep Time",
    y        = "Conversion Rate",
    fill     = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold")
  )

# I plot preparation time distributions by outcome using a beeswarm + boxplot
ggplot(wwc_tempo, aes(is_goal, duration, color = time_category)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, width = 0.4) +
  geom_beeswarm(cex = 2, alpha = 0.7) +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Duration by Outcome",
    subtitle = "Instant vs. delayed shots",
    x        = "Shot Outcome",
    y        = "Preparation Time (s)",
    color    = "Time Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )
