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
    title    = "Four Factor Clutch Score of Top 15 Shooters",
    subtitle = "Based on late-game, under pressure, one-on-one, and instant shots",
    x        = "Player",
    y        = "Clutch Score (0–1)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold")
  )



# Plot 2: Efficiency Comparison vs. All Players
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
# after computing player_clutch as before…

# 6) hard-code post-WWC goals, assists & games
post_wwc_stats <- tribble(
  ~player.name,           ~goals_since_WWC, ~assists_since_WWC, ~games_since_WWC,
  "Salma Paralluelo",            41,                12,                 49,
  "Lindsey Michelle Horan",      27,                13,                 77,
  "Alessia Russo",               35,                 5,                 59,
  "Jill Roord",                  16,                 5,                 39,
  "Thembi Kgatlana",              9,                 2,                 20,
  "Aitana Bonmatí",              34,                31,                 75,
  "Alex Morgan",                  5,                 5,                 33,
  "Esther Gonzalez",             27,                 5,                 44,
  "Alba Redondo",                38,                 5,                 50,
  "Mary Fowler",                 12,                10,                 51,
  "Lieke Martens",                3,                 3,                 11,
  "Kadidiatou Diani",            33,                22,                 57,
  "Lauren Hemp",                 15,                10,                 34,
  "Jennifer Hermoso",             2,                 1,                  8
)

# 7) compute their goal+assist per game
post_wwc_stats <- post_wwc_stats %>%
  mutate(contrib_per_game = (goals_since_WWC + assists_since_WWC) / games_since_WWC)

# 8) merge clutch scores with their post-WWC output
comparison_df2 <- player_clutch %>%
  inner_join(post_wwc_stats, by = "player.name")

# …after computing player_clutch and your post_wwc_stats tribble…

library(tidyr)  # for replace_na()

# merge clutch scores with full post-WWC stats, keep everyone
comparison_df2 <- player_clutch %>%
  left_join(post_wwc_stats, by = "player.name") %>%
  mutate(
    contrib_per_game = (goals_since_WWC + assists_since_WWC) / games_since_WWC,
    contrib_per_game = replace_na(contrib_per_game, 0)    # show missing as 0
  )

# plot Clutch Score vs. post-WWC contributions per game, all 15 players included
ggplot(comparison_df2, aes(contrib_per_game, clutch_score, label = player.name)) +
  geom_point(color = "#2c7fb8", size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray50") +
  geom_text_repel(size = 3, box.padding = 0.3) +
  scale_x_continuous(name = "Goals+Assists per Game",
                     limits = c(0, max(comparison_df2$contrib_per_game) * 1.1)) +
  scale_y_continuous(name = "Clutch Score (0–1)", limits = c(0, 1)) +
  labs(
    title    = "Clutch Score vs. Post-WWC Contributions per Game",
    subtitle = "All Top 15 Players (zero = no post-WWC data)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Compute and visualize 5-Factor Clutch Score
library(tidyverse)
library(scales)

# Load and flag data
wwc <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv") %>%
  mutate(
    late_game      = period == 2 & minute >= 80,
    under_pressure = coalesce(under_pressure, FALSE),
    instant        = duration < 0.3,
    high_quality   = DistToGoal <= 12,
    elim_stage     = period >= 3,
    is_goal        = shot.outcome.name == "Goal"
  )

# Top 15 by goals
top15 <- wwc %>%
  count(player.name, wt = is_goal) %>%
  rename(goals = n) %>%
  arrange(desc(goals)) %>%
  slice_head(n = 15)

# Compute rates
clutch5 <- wwc %>%
  filter(player.name %in% top15$player.name) %>%
  group_by(player.name) %>%
  summarise(
    late_rate    = mean(is_goal[late_game], na.rm=TRUE),
    press_rate   = mean(is_goal[under_pressure], na.rm=TRUE),
    inst_rate    = mean(is_goal[instant], na.rm=TRUE),
    quality_rate = mean(is_goal[high_quality], na.rm=TRUE),
    elim_rate    = mean(is_goal[elim_stage], na.rm=TRUE),
    .groups      = "drop"
  ) %>%
  replace_na(list(late_rate=0, press_rate=0, inst_rate=0, quality_rate=0, elim_rate=0))

# Safe normalize
safe_normalize <- function(x) {
  r <- range(x, na.rm=TRUE)
  if (diff(r)==0) return(rep(0, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

# Normalize & composite
clutch5 <- clutch5 %>%
  mutate(
    late_n    = safe_normalize(late_rate),
    press_n   = safe_normalize(press_rate),
    inst_n    = safe_normalize(inst_rate),
    quality_n = safe_normalize(quality_rate),
    elim_n    = safe_normalize(elim_rate),
    clutch5   = rowMeans(across(late_n:elim_n))
  )

# Merge and plot
df <- top15 %>%
  left_join(clutch5 %>% select(player.name, clutch5), by="player.name") %>%
  mutate(player.name = fct_reorder(player.name, goals))

ggplot(df) +
  geom_col(aes(player.name, goals / max(goals), fill="Goals"),
           width=0.35, position=position_nudge(x=-0.17)) +
  geom_col(aes(player.name, clutch5, fill="Clutch Score"),
           width=0.35, position=position_nudge(x=+0.17)) +
  geom_text(aes(player.name, goals/max(goals)-0.03, label=goals),
            size=3, position=position_nudge(x=-0.17)) +
  geom_text(aes(player.name, clutch5+0.03, label=percent(clutch5,1)),
            size=3, position=position_nudge(x=+0.17)) +
  scale_y_continuous(
    name = "Clutch Score (0–1)",
    limits = c(0,1.1),
    sec.axis = sec_axis(~ . * max(df$goals), name="Total Goals")
  ) +
  scale_fill_manual("", values=c("Goals"="#1f78b4","Clutch Score"="#e31a1c")) +
  coord_flip() +
  labs(
    title    = "Top 15 Goal-Scorers: Goals vs. 5-Factor Clutch Score",
    subtitle = "Clutch factors: late-game, under pressure, instant, quality, elimination"
  ) +
  theme_minimal(base_size=14) +
  theme(legend.position="top", plot.title=element_text(face="bold"))