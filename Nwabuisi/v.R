library(tidyverse)
library(ggrepel)

# 1) Read the dataset
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")

# 2) Compute per-team statistics
team_stats <- wwc_shots %>%
  group_by(possession_team.name) %>%
  summarise(
    avg_dist    = mean(DistToGoal, na.rm = TRUE),
    conv_rate   = mean(shot.outcome.name == "Goal", na.rm = TRUE),
    shot_count  = n(),
    .groups     = "drop"
  )

# 3) Create the scatter‐bubble plot
ggplot(team_stats, aes(x = avg_dist, y = conv_rate, size = shot_count)) +
  # Trend line
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey50") +
  # Bubbles for each team
  geom_point(alpha = 0.7, color = "#2c7fb8") +
  # Team labels
  geom_text_repel(aes(label = possession_team.name), size = 3, max.overlaps = 15) +
  # Scale adjustments
  scale_size_continuous(range = c(3, 12)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Labels and theme
  labs(
    title    = "Team Average Shot Distance vs. Goal Conversion Rate",
    subtitle = "Bubble size reflects total shot attempts",
    x        = "Average Shot Distance (m)",
    y        = "Goal Conversion Rate (%)",
    size     = "Total Shots"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )

# Install + load tidyverse if needed

# 2) Filter to Open Play vs. Counter and bin possession duration
wwc <- wwc_shots %>%
  filter(play_pattern.name %in% c("Open Play", "From Counter"),
         !is.na(TimeInPoss),
         !is.na(shot.outcome.name)) %>%
  mutate(
    poss_bin = cut(
      TimeInPoss,
      breaks = c(0, 3, 10, 20, Inf),
      labels = c("0–3s (Fast)", "3–10s (Normal)", "10–20s (Build-up)", ">20s (Slow)"),
      right = FALSE
    ),
    is_goal = (shot.outcome.name == "Goal")
  )

# 3) Summarize conversion rates by possession bin and play pattern
summary_df <- wwc %>%
  group_by(play_pattern.name, poss_bin) %>%
  summarise(
    conversion_rate = mean(is_goal, na.rm = TRUE),
    attempts        = n(),
    .groups         = "drop"
  )

# 4) Plot: Conversion rate vs. possession duration bins
ggplot(summary_df, aes(x = poss_bin, y = conversion_rate, group = play_pattern.name, color = play_pattern.name)) +
  geom_line(size = 1.2) +
  geom_point(aes(size = attempts), alpha = 0.8) +
  scale_color_manual(values = c("Open Play" = "#1f78b4", "From Counter" = "#e31a1c")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_size(range = c(3, 8)) +
  labs(
    title    = "Patience vs. Urgency: Shot Conversion by Possession Duration",
    subtitle = "Comparing Open-Play build-ups with Rapid Counterattacks",
    x        = "Possession Duration Before Shot",
    y        = "Goal Conversion Rate",
    color    = "Play Pattern",
    size     = "Number of Attempts"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# Install + load required packages
library(ggbeeswarm)

# 2) Prepare data: flag instant (first-touch), normal, and delayed shots
wwc_instinct <- wwc_shots %>%
  filter(!is.na(duration), !is.na(shot.outcome.name)) %>%
  mutate(
    time_category = case_when(
      duration < 0.3 ~ "Instant (<0.3s)",
      duration > 1.0 ~ "Delayed (>1.0s)",
      TRUE           ~ "Normal (0.3–1.0s)"
    ) %>% factor(levels = c("Instant (<0.3s)", "Normal (0.3–1.0s)", "Delayed (>1.0s)")),
    is_goal = if_else(shot.outcome.name == "Goal", "Goal", "No Goal") %>%
      factor(levels = c("No Goal", "Goal"))
  )

# 3) Summarize conversion rates by time category
conversion_summary <- wwc_instinct %>%
  group_by(time_category) %>%
  summarise(
    attempts      = n(),
    goals         = sum(is_goal == "Goal"),
    conv_rate     = goals / attempts
  )

# 4) Visualization 1: Conversion Rate Bar Chart
ggplot(conversion_summary, aes(x = time_category, y = conv_rate, fill = time_category)) +
  geom_col(color = "white", width = 0.7) +
  geom_text(aes(label = scales::percent(conv_rate, accuracy = 1)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0.02)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title    = "Goal Conversion by Shot Preparation Time",
    subtitle = "Instant one-touch vs. normal vs. delayed shots",
    x        = "Shot Preparation Time",
    y        = "Conversion Rate",
    fill     = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 15, hjust = 1)
  )

# 5) Visualization 2: Duration Distribution by Outcome (Beeswarm + Boxplot)
ggplot(wwc_instinct, aes(x = is_goal, y = duration, color = time_category)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, width = 0.4) +
  geom_beeswarm(cex = 2, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  coord_flip() +
  labs(
    title    = "Shot Duration Distribution by Outcome",
    subtitle = "Comparing one-touch (‘Instant’) to delayed shots",
    x        = "Shot Outcome",
    y        = "Preparation Time (seconds)",
    color    = "Time Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )


# 1) Read + prepare the data (as before)
wwc_instinct <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv") %>%
  filter(!is.na(duration), !is.na(shot.outcome.name)) %>%
  mutate(
    time_category = case_when(
      duration < 0.3 ~ "Instant (<0.3s)",
      duration > 1.0 ~ "Delayed (>1.0s)",
      TRUE           ~ "Normal (0.3–1.0s)"
    ) %>% 
      factor(levels = c("Instant (<0.3s)","Normal (0.3–1.0s)","Delayed (>1.0s)")),
    is_goal = if_else(shot.outcome.name == "Goal","Goal","No Goal")
  )

# 2) Plot: violins + boxplots of duration by time_category, faceted by outcome
ggplot(wwc_instinct, aes(x = duration, y = time_category, fill = time_category)) +
  geom_violin(alpha = 0.6, color = NA) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
  facet_wrap(~ is_goal, ncol = 1) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title    = "Shot Preparation Time by Category and Outcome",
    subtitle = "Violins show full distribution; boxes show median & IQR",
    x        = "Preparation Time (seconds)",
    y        = "Time Category",
    fill     = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text      = element_text(face = "bold"),
    panel.spacing   = unit(1, "lines")
  )

# 1) Install + load necessary packages
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("ggsoccer", quietly = TRUE)) install.packages("ggsoccer")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")

library(ggsoccer)
library(gridExtra)

# 2) Read the WWC shots data
wwc <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")

# 3) Compute per-player shot & goal counts
player_stats <- wwc %>%
  group_by(player.name) %>%
  summarise(
    shot_count = n(),
    goal_count = sum(shot.outcome.name == "Goal", na.rm = TRUE),
    conversion = goal_count / shot_count,
    .groups = "drop"
  )

# 4) Extract one-shot wonders (1 shot, 1 goal) and their shot details
one_shot <- player_stats %>%
  filter(shot_count == 1, goal_count == 1) %>%
  inner_join(
    wwc %>% filter(shot.outcome.name == "Goal") %>%
      select(player.name, location.x, location.y, minute, second, play_pattern.name),
    by = "player.name"
  ) %>%
  mutate(
    time_of_goal = minute + second / 60
  )

# ─────────────────────────────────────────────────────────────────────────────
# Plot 1: Bar Chart of Goal Timings
# ─────────────────────────────────────────────────────────────────────────────

p1 <- ggplot(one_shot, 
             aes(x = reorder(player.name, time_of_goal), 
                 y = time_of_goal, 
                 fill = play_pattern.name)) +
  geom_col(color = "white", width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", name = "Play Pattern") +
  labs(
    title    = "One-Shot Wonders: Goal Timing",
    subtitle = "Players who scored with their only shot of the tournament",
    x        = "Player",
    y        = "Match Minute of Goal",
    caption  = "Data: 2023 FIFA WWC open-play shots"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ─────────────────────────────────────────────────────────────────────────────
# Plot 2: Pitch Map of Scoring Locations
# ─────────────────────────────────────────────────────────────────────────────

p2 <- ggplot(one_shot) +
  annotate_pitch(dimensions = pitch_statsbomb, fill="#F5F5F5", colour="#BBBBBB") +
  geom_point(aes(x = location.x, y = location.y, color = player.name), size = 4) +
  geom_text(aes(x = location.x, y = location.y, label = player.name),
            vjust = -1, size = 3) +
  scale_color_viridis_d(name = "Player") +
  labs(
    title    = "Where the One-Shot Wonders Scored",
    subtitle = "Pitch coordinates of each lone-shot goal",
    x        = "Pitch X (meters)",
    y        = "Pitch Y (meters)"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "none"
  )

# ─────────────────────────────────────────────────────────────────────────────
# Plot 3: Conversion by Shot-Count Category
# ─────────────────────────────────────────────────────────────────────────────

conversion_comparison <- player_stats %>%
  mutate(
    category = case_when(
      shot_count == 1           ~ "One-Shot",
      shot_count <= 5           ~ "2–5 Shots",
      shot_count <= 10          ~ "6–10 Shots",
      TRUE                       ~ "11+ Shots"
    )
  )

p3 <- ggplot(conversion_comparison, 
             aes(x = category, y = conversion, fill = category)) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.2, outlier.size = 1, alpha = 0.8) +
  scale_fill_brewer(palette = "Pastel1", name = "Category") +
  scale_y_continuous(labels = scales::percent_format(1)) +
  labs(
    title    = "Efficiency by Shot-Count Category",
    subtitle = "One-Shot Wonders vs. Regular Shooters",
    x        = "Shots Taken in Tournament",
    y        = "Goal Conversion Rate",
    caption  = "Violin = distribution; Box = median & IQR"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x     = element_text(angle = 15, hjust = 1)
  )

# ─────────────────────────────────────────────────────────────────────────────
# Arrange and Display the Three Plots
# ─────────────────────────────────────────────────────────────────────────────

grid.arrange(p1, p2, p3, nrow = 3)

# 1) Read the data


# 2) Compute per‐player stats and isolate one‐shot wonders
player_stats <- wwc %>%
  group_by(player.name) %>%
  summarise(
    shot_count = n(),
    goal_count = sum(shot.outcome.name == "Goal", na.rm=TRUE),
    .groups = "drop"
  )

one_shot <- player_stats %>%
  filter(shot_count == 1, goal_count == 1) %>%
  inner_join(
    wwc %>% filter(shot.outcome.name == "Goal") %>%
      select(player.name, location.x, location.y, minute, second, play_pattern.name),
    by = "player.name"
  ) %>%
  mutate(match_time = minute + second / 60)

# ─────────────────────────────────────────────────────────────────────────────
# Plot 1: Goal Timing (Bar) – save/view separately
# ─────────────────────────────────────────────────────────────────────────────
p1 <- ggplot(one_shot, 
             aes(x = reorder(player.name, match_time), 
                 y = match_time, 
                 fill = play_pattern.name)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", name = "Play Pattern") +
  labs(
    title    = "One‐Shot Wonders: When They Scored",
    subtitle = "Each scored on their only attempt",
    x        = NULL,
    y        = "Match Time (minutes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )

print(p1)
# ggsave("one_shot_timing.pdf", p1, width=8, height=5)

# ─────────────────────────────────────────────────────────────────────────────
# Plot 2: Pitch Scatter (Basic axes) – save/view separately
# ─────────────────────────────────────────────────────────────────────────────
p2 <- ggplot(one_shot, aes(x = location.x, y = location.y, color = player.name)) +
  geom_point(size = 4) +
  geom_text(aes(label = player.name), vjust = -1, size = 3) +
  scale_color_viridis_d(option = "plasma", name = "Player") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "One‐Shot Wonders: Where They Scored",
    x     = "Pitch X (meters)",
    y     = "Pitch Y (meters)",
    caption = "Field dimensions: 120m × 80m"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold")
  )

print(p2)
# ggsave("one_shot_pitchmap.pdf", p2, width=8, height=6)

# ─────────────────────────────────────────────────────────────────────────────
# Plot 3: Conversion by Volume (Bar) – save/view separately
# ─────────────────────────────────────────────────────────────────────────────
conversion_comparison <- player_stats %>%
  mutate(category = case_when(
    shot_count == 1  ~ "One‐Shot",
    shot_count <= 5  ~ "2–5 Shots",
    shot_count <= 10 ~ "6–10 Shots",
    TRUE             ~ "11+ Shots"
  )) %>%
  group_by(category) %>%
  summarise(avg_conv = mean(goal_count / shot_count), .groups="drop")

p3 <- ggplot(conversion_comparison, aes(x = category, y = avg_conv, fill = category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(avg_conv, 1)), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Pastel1", guide = FALSE) +
  scale_y_continuous(labels = scales::percent_format(1), expand = c(0,0.02)) +
  labs(
    title    = "Goal Conversion by Shot‐Count Category",
    subtitle = "One‐Shot Wonders at perfect 100%",
    x        = "Shots Taken",
    y        = "Average Conversion Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

print(p3)
# ggsave("one_shot_conversion.pdf", p3, width=8, height=5)
# ─────────────────────────────────────────────────────────────────────────────
# Superstar Shot Paradox Visualizations
# ─────────────────────────────────────────────────────────────────────────────
library(viridis)


# 3) Compute per-player stats: total shots, total goals, and conversion rate
player_stats <- wwc_shots %>%
  group_by(player.name) %>%
  summarise(
    shots    = n(),
    goals    = sum(shot.outcome.name == "Goal", na.rm = TRUE),
    conversion = goals / shots,
    .groups  = "drop"
  )

# 4) Identify the top 15 players by shot volume
top15 <- player_stats %>%
  arrange(desc(shots)) %>%
  slice_head(n = 15)

# ─────────────────────────────────────────────────────────────────────────────
# Plot 1: Shots vs. Goals Scatter
# ─────────────────────────────────────────────────────────────────────────────
p1 <- ggplot(player_stats, aes(x = shots, y = goals)) +
  geom_point(color = "#777777", alpha = 0.4) +
  geom_point(data = top15, aes(color = player.name), size = 3) +
  geom_text_repel(data = top15, aes(label = player.name),
                  size = 3, box.padding = 0.3, max.overlaps = 10) +
  scale_color_viridis_d(option = "plasma", name = "Top 15 Shooters") +
  labs(
    title    = "Superstar Shot Paradox: Shots vs. Goals",
    subtitle = "Top shooters (colored) fired many shots but scored few goals",
    x        = "Total Shots Attempted",
    y        = "Total Goals Scored"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold")
  )

print(p1)


# ─────────────────────────────────────────────────────────────────────────────
# Plot 2: Conversion Rate Bar Chart for Top Shooters
# ─────────────────────────────────────────────────────────────────────────────
p2 <- top15 %>%
  arrange(conversion) %>%
  mutate(player.name = fct_inorder(player.name)) %>%
  ggplot(aes(x = player.name, y = conversion, fill = conversion)) +
  geom_col() +
  geom_text(aes(label = scales::percent(conversion, accuracy = 1)),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_viridis_c(option = "inferno", direction = -1, guide = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, .1))) +
  labs(
    title    = "Conversion Rates of the 15 Highest-Volume Shooters",
    subtitle = "Notice stars with low conversion despite many attempts",
    x        = NULL,
    y        = "Goal Conversion Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(p2)



p3 <- ggplot(player_stats, aes(x = shots, y = conversion)) +
  geom_bin2d(bins = 30) +
  scale_fill_viridis_c(option = "magma", name = "Player Count") +
  geom_point(data = top15, aes(color = player.name), size = 2) +
  geom_text_repel(data = top15, aes(label = player.name),
                  size = 3, box.padding = 0.2, max.overlaps = 10) +
  labs(
    title    = "Where Superstars Sit on the Shots–Conversion Map",
    subtitle = "Heatmap shows most players’ balance of volume vs. efficiency",
    x        = "Total Shots Attempted",
    y        = "Goal Conversion Rate",
    color    = "Top 15 Shooters"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold")
  )

print(p3)
library(cowplot)


# 3) Compute per-player stats
player_stats <- wwc_shots %>%
  group_by(player.name) %>%
  summarise(
    shots      = n(),
    goals      = sum(shot.outcome.name == "Goal", na.rm=TRUE),
    conversion = goals / shots,
    .groups    = "drop"
  )

# 4) Define “superstars” as the top 15 by shot volume
top15 <- player_stats %>%
  arrange(desc(shots)) %>%
  slice_head(n = 15) %>%
  pull(player.name)

# 5) Add category flag
player_stats <- player_stats %>%
  mutate(
    category = if_else(player.name %in% top15, "Superstar (Top 15)", "Other Players")
  )

# ─────────────────────────────────────────────────────────────────────────────
# Plot A: Boxplot + Jitter of Conversion Rates
# ─────────────────────────────────────────────────────────────────────────────
p_box <- ggplot(player_stats, aes(x = category, y = conversion, color = category)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  scale_color_manual(values = c("Superstar (Top 15)" = "#d95f02", "Other Players" = "#1b9e77")) +
  scale_y_continuous(labels = scales::percent_format(1), limits = c(0,NA)) +
  labs(
    title    = "Conversion Rate: Superstars vs. Other Players",
    subtitle = "Distribution of goal conversion rates across player categories",
    x        = NULL,
    y        = "Goal Conversion Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold")
  )

# ─────────────────────────────────────────────────────────────────────────────
# Plot B: Mean Conversion with 95% CI
# ─────────────────────────────────────────────────────────────────────────────
summary_cat <- player_stats %>%
  group_by(category) %>%
  summarise(
    mean_conv = mean(conversion),
    sd_conv   = sd(conversion),
    n         = n(),
    se_conv   = sd_conv / sqrt(n),
    lower_ci  = mean_conv - 1.96 * se_conv,
    upper_ci  = mean_conv + 1.96 * se_conv,
    .groups   = "drop"
  )

p_bar <- ggplot(summary_cat, aes(x = category, y = mean_conv, fill = category)) +
  geom_col(width = 0.5, alpha = 0.8) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_fill_manual(values = c("Superstar (Top 15)" = "#d95f02", "Other Players" = "#1b9e77")) +
  scale_y_continuous(labels = scales::percent_format(1), limits = c(0, NA)) +
  geom_text(aes(label = scales::percent(mean_conv, 1)), 
            vjust = -0.5, size = 4) +
  labs(
    title    = "Average Conversion Rate by Player Category",
    subtitle = "Superstars vs. the Rest with 95% CI",
    x        = NULL,
    y        = "Mean Conversion Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold")
  )

# ─────────────────────────────────────────────────────────────────────────────
# Display Plots Side by Side
# ─────────────────────────────────────────────────────────────────────────────
plot_grid(p_box, p_bar, ncol = 2, labels = c("A", "B"))
# ─────────────────────────────────────────────────────────────────────────────
# Low Shots vs. High Shots: Conversion Rate & Goal Map
# ─────────────────────────────────────────────────────────────────────────────

# 1) Install + load tidyverse if needed

# 2) Read the data


# 3) Prepare: categorize by shot end‐height, filter out missing heights
wwc_height <- wwc %>%
  filter(!is.na(shot.end_location.z)) %>%
  mutate(
    height_cat = if_else(
      shot.end_location.z <= 1.0,
      "Low Shots (≤1m)",
      "High Shots (>1m)"
    ),
    is_goal = (shot.outcome.name == "Goal")
  )

# ─────────────────────────────────────────────────────────────────────────────
# Plot 1: Side‐by‐Side Bar Chart of Conversion Rates
# ─────────────────────────────────────────────────────────────────────────────
conversion_df <- wwc_height %>%
  group_by(height_cat) %>%
  summarise(
    attempts  = n(),
    goals     = sum(is_goal),
    conv_rate = goals / attempts,
    .groups   = "drop"
  )

ggplot(conversion_df, aes(x = height_cat, y = conv_rate, fill = height_cat)) +
  geom_col(width = 0.6, color = "white") +
  geom_text(aes(label = scales::percent(conv_rate, accuracy = 1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title    = "Goal Conversion: Low Shots vs. High Shots",
    subtitle = "Shots ending ≤1m off the ground vs. >1m",
    x        = NULL,
    y        = "Conversion Rate",
    fill     = "Shot Height"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 15, hjust = 1)
  )

goals_df <- wwc_height %>% filter(is_goal)

ggplot(goals_df, aes(x = shot.end_location.x, y = shot.end_location.y, color = height_cat)) +
  geom_point(size = 3, alpha = 0.8) +
  coord_fixed(xlim = c(100, 120), ylim = c(0, 80), expand = FALSE) +
  scale_color_brewer(palette = "Set1", name = "Shot Height") +
  labs(
    title    = "Where Low vs. High Shots Found the Net",
    subtitle = "Goal locations by end‐height category",
    x        = "Pitch X Coordinate (m)",
    y        = "Pitch Y Coordinate (m)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom")


library(plotly)

# Load and prepare data

wwc3d <- na.omit(wwc[, c("shot.end_location.x", "shot.end_location.y", "shot.end_location.z", "shot.outcome.name")])
wwc3d$is_goal <- wwc3d$shot.outcome.name == "Goal"

# Define pitch plane at z=0
plane <- list(
  x = c(0, 120, 120, 0),
  y = c(0, 0, 80, 80),
  z = c(0, 0, 0, 0)
)

# Build the figure
fig <- plot_ly() %>%
  # Pitch surface
  add_trace(
    type = 'mesh3d',
    x = plane$x, y = plane$y, z = plane$z,
    color = I('darkgreen'),
    opacity = 0.25,
    showscale = FALSE
  ) %>%
  # Shot endpoints
  add_trace(
    data = wwc3d,
    x = ~shot.end_location.x,
    y = ~shot.end_location.y,
    z = ~shot.end_location.z,
    mode = 'markers',
    type = 'scatter3d',
    marker = list(
      size = 3,
      color = ifelse(wwc3d$is_goal, 'blue', 'red'),
      opacity = 0.8
    ),
    hovertemplate = ~paste(
      "X: ", round(shot.end_location.x,1), "<br>",
      "Y: ", round(shot.end_location.y,1), "<br>",
      "Height (Z): ", round(shot.end_location.z,1), "<br>",
      "Outcome: ", shot.outcome.name, "<extra></extra>"
    )
  ) %>%
  layout(
    title = "3D Map of Shot End Locations (X/Y) and Height (Z)",
    scene = list(
      xaxis = list(title = "X (meters)"),
      yaxis = list(title = "Y (meters)"),
      zaxis = list(title = "Z (meters)", range = c(0, max(wwc3d$shot.end_location.z))),
      aspectmode = 'manual',
      aspectratio = list(x=1.5, y=1, z=0.5)
    ),
    margin = list(l=0, r=0, b=0, t=50)
  )



df <- wwc %>%
  filter(!is.na(shot.end_location.y), !is.na(shot.end_location.z),
         shot.end_location.x >= 118) %>%
  mutate(
    # Divide horizontally at y = 40 (center of goal)
    horiz = if_else(shot.end_location.y < 40, "Left", "Right"),
    # Divide vertically at the median shot height among these shots
    vert = if_else(
      shot.end_location.z < median(shot.end_location.z, na.rm=TRUE),
      "Low", "High"
    ),
    quadrant = paste(vert, horiz)
  )

# 4) Summarize conversion rate by quadrant
quad_summary <- df %>%
  group_by(horiz, vert) %>%
  summarise(
    attempts  = n(),
    goals     = sum(shot.outcome.name == "Goal", na.rm=TRUE),
    conv_rate = goals / attempts,
    .groups   = "drop"
  )

# 5) Heatmap of conversion rates across the 4 quadrants
ggplot(quad_summary, aes(x = horiz, y = vert, fill = conv_rate)) +
  geom_tile(color = "white", size = 0.8) +
  geom_text(aes(label = sprintf("%d/%d\n%.1f%%", goals, attempts, conv_rate*100)),
            color = "black", size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#fee8c8", high = "#e34a33", 
                      labels = scales::percent_format(1),
                      name = "Conv. Rate") +
  labs(
    title    = "Goal Conversion by Goal-Mouth Quadrant",
    subtitle = "Upper vs. Lower and Left vs. Right sections of the goal",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    axis.text       = element_text(face = "bold"),
    panel.grid      = element_blank()
  )



df <- wwc %>%
  filter(!is.na(shot.end_location.y),
         !is.na(shot.end_location.z),
         shot.end_location.x >= 118) %>%
  mutate(
    horiz = if_else(shot.end_location.y < 40, "Left", "Right"),
    vert  = if_else(
      shot.end_location.z < median(shot.end_location.z, na.rm=TRUE),
      "Low", "High"
    ),
    vert  = factor(vert, levels = c("Low","High")),
    horiz = factor(horiz, levels = c("Left","Right")),
    quadrant = paste(vert, horiz)
  )

quad_summary <- df %>%
  group_by(horiz, vert) %>%
  summarise(
    attempts  = n(),
    goals     = sum(shot.outcome.name == "Goal", na.rm=TRUE),
    conv_rate = goals / attempts,
    .groups   = "drop"
  )

library(factoextra)

# 2) Read and prepare data

cluster_df <- wwc %>%
  filter(!is.na(duration), !is.na(DistToGoal), !is.na(DefendersInCone)) %>%
  mutate(
    is_goal = as.numeric(shot.outcome.name == "Goal")
  ) %>%
  select(duration, DistToGoal, DefendersInCone, is_goal)

# 3) Scale features
scaled_df <- scale(cluster_df)

# 4) Determine a good k (optional elbow plot)
fviz_nbclust(scaled_df, kmeans, method = "wss") + 
  labs(subtitle = "Elbow Method for Optimal k")

# 5) Run K-means (here k = 3)
set.seed(123)
km_res <- kmeans(scaled_df, centers = 3, nstart = 25)

# 6) Attach cluster labels back to the original filtered data
plot_df <- wwc %>%
  filter(!is.na(duration), !is.na(DistToGoal), !is.na(DefendersInCone)) %>%
  mutate(cluster = factor(km_res$cluster),
         is_goal = shot.outcome.name == "Goal")

# 7) 2D scatter: duration vs distance, colored by cluster, shaped by goal
ggplot(plot_df, aes(x = duration, y = DistToGoal, color = cluster, shape = is_goal)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16), labels = c("No Goal","Goal")) +
  scale_color_brewer(palette = "Set2", name = "Cluster") +
  labs(
    title    = "K-Means Clusters of Shots by Tempo & Distance",
    subtitle = "Clusters characterized by shot release time, distance, and defenders nearby",
    x        = "Shot Release Time (sec)",
    y        = "Distance to Goal (m)",
    shape    = "Outcome"
  ) +
  theme_minimal(base_size = 14)

# 8) Cluster profiles: mean feature values + conversion rate per cluster
profile <- plot_df %>%
  group_by(cluster) %>%
  summarise(
    avg_duration       = mean(duration),
    avg_distance       = mean(DistToGoal),
    avg_defenders      = mean(DefendersInCone),
    conv_rate          = mean(is_goal),
    count              = n()
  )

# 9) Profile visualization: radar or bar chart
# Here: bar chart of conversion rate by cluster
ggplot(profile, aes(x = cluster, y = conv_rate, fill = cluster)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(conv_rate, 1)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2", guide = FALSE) +
  labs(
    title   = "Goal Conversion Rate by Cluster",
    subtitle= "How successful each shot-type cluster is",
    x       = "Cluster",
    y       = "Conversion Rate"
  ) +
  theme_minimal(base_size = 14)
# ─────────────────────────────────────────────────────────────────────────────
# PCA & Unsupervised Learning on WWC Shot Data
# ─────────────────────────────────────────────────────────────────────────────

library(factoextra)
library(cluster)

# 2) Read & prepare data for unsupervised learning


# Select numerical features of interest (filter NAs)
unsup_df <- wwc %>%
  select(duration, DistToGoal, AngleToGoal, DefendersInCone, avevelocity) %>%
  drop_na()

# 3) Scale features
scaled_df <- scale(unsup_df)

# ─────────────────────────────────────────────────────────────────────────────
# PRINCIPAL COMPONENT ANALYSIS
# ─────────────────────────────────────────────────────────────────────────────
pca_res <- prcomp(scaled_df, center = TRUE, scale. = FALSE)

# 3a) Scree plot to choose number of PCs
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "PCA Scree Plot: Variance Explained by PCs")

# 3b) Biplot of first two PCs
fviz_pca_biplot(pca_res, 
                geom.ind = "point", 
                pointshape = 21, 
                pointsize = 2, 
                fill.ind = "lightblue", 
                col.var = "red",
                repel = TRUE) +
  labs(title = "PCA Biplot of Shot Features",
       subtitle = "Points = shots; Arrows = feature loadings")

pca_coords <- as.data.frame(pca_res$x[,1:3])

# Determine optimal k (elbow + silhouette)
fviz_nbclust(pca_coords, kmeans, method = "wss") + labs(subtitle="Elbow Method")
fviz_nbclust(pca_coords, kmeans, method = "silhouette") + labs(subtitle="Silhouette Method")

# Run K-means with chosen k (e.g. 3)
set.seed(42)
km_pca <- kmeans(pca_coords, centers = 3, nstart = 25)

# Visualize K-means on PCA1 vs PCA2
fviz_cluster(km_pca, data = pca_coords,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-Means Clusters on PCA Coordinates")

# ─────────────────────────────────────────────────────────────────────────────
# HIERARCHICAL CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute distance matrix
dist_mat <- dist(scaled_df, method = "euclidean")

# Hierarchical clustering
hc_res <- hclust(dist_mat, method = "ward.D2")

# Dendrogram
fviz_dend(hc_res, k = 4,  # cut tree into 4 groups
          cex = 0.5,
          k_colors = "jco",
          color_labels_by_k = TRUE,
          main = "Hierarchical Clustering Dendrogram")

# CLUSTER PROFILES
profile_df <- as_tibble(scaled_df) %>%
  mutate(cluster = factor(km_pca$cluster))

# Compute mean feature values by cluster
cluster_profiles <- profile_df %>%
  group_by(cluster) %>%
  summarise_all(mean) %>%
  pivot_longer(-cluster, names_to="feature", values_to="mean_value")

# Bar chart of feature means by cluster
ggplot(cluster_profiles, aes(x = feature, y = mean_value, fill = cluster)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title    = "Cluster Profiles: Mean Scaled Feature Values",
    x        = "Feature",
    y        = "Average (Z-score)",
    fill     = "Cluster"
  ) +
  theme_minimal(base_size = 14)

