library(dplyr)
library(tidyverse)
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")

# Filters goalies to only those with 25 shots against
goalies=wwc_shots %>% count(player.name.GK) %>% filter(n>=25)
goalies=goalies$player.name.GK[1:length(goalies$player.name.GK)]

# Bar plot that shows total shots and the outcome
wwc_shots %>%
  mutate(Goal = ifelse(shot.outcome.name == "Goal", 'yes', 'no')) %>%
  filter(shot.outcome.name != "Goal") %>%
  filter(player.name.GK %in% goalies) %>%
  mutate(last_name = sapply(strsplit(player.name.GK, " "), function(x) tail(x, 1))) %>%
  ggplot(aes(x=last_name))+
  geom_bar(aes(colour = shot.outcome.name, fill=shot.outcome.name))

# Bar plot that shows percentage outcome
wwc_shots %>%
  mutate(Goal = ifelse(shot.outcome.name == "Goal", 'yes', 'no')) %>%
  filter(shot.outcome.name != "Goal") %>%
  filter(player.name.GK %in% goalies) %>%
  mutate(last_name = sapply(strsplit(player.name.GK, " "), function(x) tail(x, 1))) %>%
  ggplot(aes(x=last_name))+
  geom_bar(aes(colour = shot.outcome.name, fill=shot.outcome.name), position="fill")

# Every single shot that was either saved or a goal (no blocks, off target, etc)
wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())  %>% 
  filter(shot.end_location.z<=3, shot.end_location.y<=45, shot.end_location.y>=36) %>% 
  filter(player.name.GK %in% goalies, shot.outcome.name%in% c("Saved", "Goal")) %>% 
  ggplot(aes(y=shot.end_location.z, x=shot.end_location.y)) +
  geom_point(aes(color=Goal, shape=Goal), alpha=0.5, size=3) 

# Every single shot that was either saved or a goal by player
wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())  %>% 
  filter(shot.end_location.z<=3, shot.end_location.y<=45, shot.end_location.y>=36) %>% 
  filter(player.name.GK %in% goalies, shot.outcome.name%in% c("Saved", "Goal")) %>% 
  ggplot(aes(y=shot.end_location.z, x=shot.end_location.y)) +
  geom_point(aes(color=Goal, shape=Goal), alpha=0.5, size=3) +
  facet_wrap(~player.name.GK)
