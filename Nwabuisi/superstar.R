# superstar_paradox.R

library(tidyverse)
library(ggrepel)
library(viridis)

# 1) Compute shots, goals, and conversion rate for every player
player_stats <- wwc_shots %>%
  group_by(player.name) %>%
  summarise(
    shots      = n(),
    goals      = sum(shot.outcome.name == "Goal", na.rm = TRUE),
    conversion = goals / shots,
    .groups    = "drop"
  )

# 2) Pick the top 15 shooters by shot volume
top15 <- player_stats %>%
  arrange(desc(shots)) %>%
  slice_head(n = 15)

# Plot A: Shots vs Goals scatter, everyone in gray, top15 in color+labels
ggplot(player_stats, aes(shots, goals)) +
  geom_point(color = "gray70", alpha = 0.5) +
  geom_point(data = top15, aes(color = player.name), size = 3) +
  geom_text_repel(data = top15, aes(label = player.name), size = 3) +
  labs(
    title = "Shots vs Goals: Highlighting the Top 15 Shooters",
    x     = "Total Shots",
    y     = "Total Goals"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

# Plot B: Horizontal bar of conversion rates for the top 15
ggplot(
  top15 %>% arrange(conversion) %>% mutate(player.name = fct_inorder(player.name)),
  aes(player.name, conversion, fill = conversion)
) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(direction = -1, guide = FALSE) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  labs(
    title = "Conversion Rates of Top 15 Shooters",
    x     = NULL,
    y     = "Conversion Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Plot C: Volume vs Efficiency map – small gray for everyone, big blue for top15, size ~ shots
ggplot(player_stats, aes(shots, conversion)) +
  geom_point(aes(size = shots), color = "gray50", alpha = 0.4) +
  geom_point(data = top15, aes(shots, conversion), color = "#2c7fb8", size = 5) +
  geom_text_repel(
    data = top15,
    aes(shots, conversion, label = player.name),
    size = 3, nudge_y = 0.02
  ) +
  scale_size_continuous(name = "Total Shots", range = c(2, 10)) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  labs(
    title = "Volume vs Efficiency Map",
    x     = "Total Shots",
    y     = "Conversion Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "right"
  )




# clutch_analysis_fixed.R

# 1) Install & load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("scales",   quietly = TRUE)) install.packages("scales")
library(tidyverse)
library(scales)
library(ggrepel)

# 2) Read shot data and flag clutch contexts
wwc <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv") %>%
  mutate(
    late_game      = (period == 2 & minute >= 80),
    under_pressure = coalesce(under_pressure, FALSE),
    one_on_one     = shot.one_on_one == 1,
    instant        = duration < 0.3,
    is_goal        = shot.outcome.name == "Goal"
  )

# 3) Compute per-player clutch metrics, then normalize & combine into a score
player_clutch <- wwc %>%
  group_by(player.name) %>%
  summarise(
    total_shots = n(),
    late_rate   = sum(late_game & is_goal) / max(sum(late_game), 1),
    press_rate  = sum(under_pressure & is_goal) / max(sum(under_pressure), 1),
    oo_rate     = sum(one_on_one & is_goal) / max(sum(one_on_one), 1),
    inst_rate   = sum(instant & is_goal) / max(sum(instant), 1),
    .groups     = "drop"
  ) %>%
  arrange(desc(total_shots)) %>%
  slice_head(n = 15) %>%
  mutate_at(vars(late_rate, press_rate, oo_rate, inst_rate),
            ~ (. - min(., na.rm = TRUE)) / diff(range(., na.rm = TRUE))) %>%
  rowwise() %>%
  mutate(clutch_score = mean(c(late_rate, press_rate, oo_rate, inst_rate), na.rm = TRUE)) %>%
  ungroup()

# 4) Plot Clutch Score as a horizontal bar
ggplot(player_clutch, aes(x = reorder(player.name, clutch_score), y = clutch_score, fill = clutch_score)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", name = "Clutch\nScore") +
  scale_y_continuous(labels = percent_format(1), limits = c(0, 1)) +
  labs(
    title    = "Clutch Score of Top 15 Shooters",
    subtitle = "Based on late-game, under pressure, one-on-one, and instant shots",
    x        = "Player",
    y        = "Clutch Score (0–1)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold")
  )

# 5) Compare Clutch Score to Post‐WWC Performance & Efficiency vs. All Players

# a) prepare post‐WWC performance ranking
postwc_rank <- tibble(
  player.name = c("Salma Paralluelo Avingono","Lindsey Michelle Horan","Alessia Russo",
                  "Jill Roord","Thembi Kgatlana","Jennifer Hermoso Fuentes",
                  "Lauren Hemp","Kadidiatou Diani","Lieke Martens","Mary Boio Fowler",
                  "Alba María Redondo Ferrer","Caitlin Jade Foord",
                  "Esther Gonzalez Rodríguez","Alexandra Morgan Carrasco",
                  "Aitana Bonmati Conca"),
  rank = 1:15
) %>%
  mutate(post_score = (16 - rank) / 15)

# b) build comparison dataframe
comparison_df <- player_clutch %>%
  inner_join(postwc_rank, by = "player.name")

# Plot 1: Clutch vs Post‐WWC performance
ggplot(comparison_df, aes(clutch_score, post_score, label = player.name)) +
  geom_point(color = "#2c7fb8", size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray50") +
  geom_text_repel(size = 3, box.padding = 0.3) +
  scale_x_continuous(labels = percent_format(1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(1), limits = c(0, 1)) +
  labs(
    title    = "Clutch Score vs. Post-WWC Performance",
    subtitle = "Do clutchiest players sustain form after the tournament?",
    x        = "Clutch Score",
    y        = "Post-WWC Performance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Plot 2: Efficiency Comparison vs. All Players (improved)
player_stats <- wwc %>%
  group_by(player.name) %>%
  summarise(
    shots      = n(),
    goals      = sum(is_goal),
    conversion = goals / shots,
    .groups    = "drop"
  ) %>%
  mutate(category = if_else(player.name %in% player_clutch$player.name,
                            "Top 15 Shooters", "Other Players"))

ggplot(player_stats, aes(category, conversion, fill = category)) +
  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point",
               shape = 21, size = 5, fill = "white", color = "black") +
  scale_fill_manual(values = c("Top 15 Shooters" = "#D95F02",
                               "Other Players"   = "#1B9E77")) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(
    title    = "Goal Conversion Efficiency: Top Shooters vs. Everyone Else",
    subtitle = "Violin = distribution; dots = individual; white dots = means",
    x        = NULL,
    y        = "Conversion Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold")
  )
