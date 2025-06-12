library(dplyr)
library(tidyverse)
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")

# Filters goalies to only those with 25 shots against
goalies=wwc_shots %>% count(player.name.GK) %>% filter(n>=30)
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



wwc_shots %>%
  mutate(Goal = ifelse(shot.outcome.name == "Goal", 'yes', 'no')) %>%
  filter(player.name.GK %in% goalies) %>%
  mutate(last_name = sapply(strsplit(player.name.GK, " "), function(x) tail(x, 1))) %>%
  ggplot(aes(x=last_name))+
  geom_bar(aes(colour = shot.outcome.name, fill=shot.outcome.name))

# Bar plot that shows percentage outcome
wwc_shots %>%
  mutate(Goal = ifelse(shot.outcome.name == "Goal", 'yes', 'no')) %>%
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
  mutate(last_name = sapply(strsplit(player.name.GK, " "), function(x) tail(x, 1))) %>%
  ggplot(aes(y=shot.end_location.z, x=shot.end_location.y)) +
  geom_point(aes(color=Goal, shape=Goal), alpha=0.5, size=3) +
  facet_wrap(~last_name)+
  theme_minimal() +
  labs(title="Goalie Performance", x="Shot Width", y="Shot Height")
  

library(ggpubr)
library(jpeg)


#Can only open with a file called goal.jpeg
# img <- readJPEG("goal.jpeg")
# 
# 
# wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())  %>% 
#   filter(shot.end_location.z<=3, shot.end_location.y<=45, shot.end_location.y>=36) %>% 
#   filter(player.name.GK %in% goalies, shot.outcome.name%in% c("Saved", "Goal", "Post")) %>% 
#   ggplot(aes(y=shot.end_location.z, x=shot.end_location.y)) +
#   background_image(img) +
#   geom_point(aes(color=Goal, shape=Goal), alpha=0.5, size=3)

# Clustering analysis
test=wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())%>% 
  filter(shot.outcome.name%in% c("Saved", "Goal")) %>% 
  filter(avevelocity<=50) %>% 
  select(avevelocity, DistSGK)

testk=test %>% mutate(DistSGK=as.numeric(scale(DistSGK)), avevolicity=as.numeric(scale(DistSGK)))




std_kmeans=testk %>% 
  kmeans(algorithm = "Lloyd",
         centers=3,
         nstart=50)

wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())%>% 
  filter(shot.outcome.name%in% c("Saved", "Goal")) %>% 
  filter(avevelocity<=50) %>%
  mutate(cluster=factor(std_kmeans$cluster)) %>% 
  ggplot(aes(DistSGK, avevelocity, color=cluster)) + 
  geom_point()+
  ggthemes::scale_color_colorblind()+
  facet_wrap(~shot.outcome.name)

wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())%>% 
  filter(shot.outcome.name%in% c("Saved", "Goal")) %>% 
  filter(avevelocity<=50) %>%
  mutate(cluster=factor(std_kmeans$cluster)) %>% 
  select(shot.outcome.name, cluster, possession_team.name) %>% 
  mutate(shot.outcome.name = ifelse(shot.outcome.name=="Goal", 1, 0)) %>% 
  group_by(cluster) %>% summarise(goal_prop= sum(shot.outcome.name)/n())

country_clusters=wwc_shots %>%
  mutate(Goal = ifelse(shot.outcome.name == "Goal", "yes", "no")) %>%
  filter(shot.outcome.name %in% c("Saved", "Goal")) %>%
  filter(avevelocity <= 50) %>%
  mutate(cluster = factor(std_kmeans$cluster)) %>%
  count(cluster, possession_team.name) %>%
  pivot_wider(names_from = possession_team.name, values_from = n, values_fill = 0)

country_clusters2=wwc_shots %>%
  mutate(Goal = ifelse(shot.outcome.name == "Goal", "yes", "no")) %>%
  filter(shot.outcome.name %in% c("Saved", "Goal")) %>%
  filter(avevelocity <= 50) %>%
  mutate(cluster = factor(std_kmeans$cluster)) %>%
  count(possession_team.name, cluster) %>%
  pivot_wider(names_from = cluster, values_from = n, values_fill = 0)




# Analysis on Moroccan GK
wwc_shots %>% filter(player.name.GK=="Khadija Er-Rmichi", 
possession_team.name %in% c("Germany Women's",
                            "Colombia Women's", "Korea Republic Women's")) %>% 
  mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())  %>% 
  filter(shot.end_location.z<=3, shot.end_location.y<=45, shot.end_location.y>=36) %>% 
  filter(shot.outcome.name%in% c("Saved", "Goal")) %>% 
  ggplot(aes(y=shot.end_location.z, x=shot.end_location.y, size=avevelocity)) +
  geom_point(aes(color=Goal, shape=Goal, size=avevelocity), alpha=0.5) +
  facet_wrap(~possession_team.name)





# All shot outcomes




test=wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())%>% 
  filter(avevelocity<=50) %>% 
  select(avevelocity, DistSGK)

testk=test %>% mutate(DistSGK=as.numeric(scale(DistSGK)), avevolicity=as.numeric(scale(DistSGK)))




std_kmeans=testk %>% 
  kmeans(algorithm = "Lloyd",
         centers=3,
         nstart=50)


wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())%>% 
  filter(avevelocity<=50) %>%
  mutate(cluster=factor(std_kmeans$cluster)) %>% 
  ggplot(aes(DistSGK, avevelocity, color=cluster)) + 
  geom_point()+
  ggthemes::scale_color_colorblind()


wwc_shots %>% mutate(Result=ifelse(shot.outcome.name=="Goal", 'Goal','No Goal'), Shots=n())%>% 
  filter(avevelocity<=50) %>%
  mutate(cluster=factor(std_kmeans$cluster)) %>% 
  ggplot(aes(DistSGK, avevelocity, color=cluster)) + 
  geom_point()+
  ggthemes::scale_color_colorblind()+
  facet_wrap(~shot.outcome.name)


wwc_shots %>% mutate(Result=ifelse(shot.outcome.name=="Goal", 'Goal','No Goal'), Shots=n())%>% 
  filter(avevelocity<=50) %>%
  mutate(cluster=factor(std_kmeans$cluster)) %>% 
  ggplot(aes(DistSGK, avevelocity, color=cluster)) + 
  geom_point()+
  ggthemes::scale_color_colorblind()+
  facet_wrap(~Result)

wwc_shots %>% mutate(Goal=ifelse(shot.outcome.name=="Goal", 'yes','no'), Shots=n())%>% 
  filter(avevelocity<=50) %>%
  mutate(cluster=factor(std_kmeans$cluster)) %>% 
  select(shot.outcome.name, cluster, possession_team.name) %>% 
  mutate(shot.outcome.name = ifelse(shot.outcome.name=="Goal", 1, 0)) %>% 
  group_by(cluster) %>% summarise(goal_prop= sum(shot.outcome.name)/n())


country_clusters2=wwc_shots %>%
  mutate(Goal = ifelse(shot.outcome.name == "Goal", "yes", "no")) %>%
  filter(avevelocity <= 50) %>%
  mutate(cluster = factor(std_kmeans$cluster)) %>%
  count(possession_team.name, cluster) %>%
  pivot_wider(names_from = cluster, values_from = n, values_fill = 0)


# Soft clustering
wwc_shots <- wwc_shots |> 
  filter(avevelocity <= 50) %>%
  mutate(
    std_velo = as.numeric(scale(avevelocity)),
    std_dist = as.numeric(scale(DistSGK))
  )

wwc_shots |> 
  ggplot(aes(x = std_dist, y = std_velo)) +
  geom_point(size = 4) +
  coord_fixed()

players_dist=wwc_shots %>% 
  filter(avevelocity <= 50) %>%
  select(std_velo, std_dist) %>% 
  dist()

players_dist_matrix = as.matrix(players_dist)




wwc_complete = players_dist %>% 
  hclust(method="complete")

wwc_shots %>% mutate(cluster=factor(cutree(wwc_complete, h=3.5))) %>% 
  ggplot(aes(std_dist, std_velo,
             color=cluster))+
  geom_point()+
  ggthemes::scale_color_colorblind()

library(ggdendro)
wwc_complete |> 
  ggdendrogram(labels = FALSE, 
               leaf_labels = FALSE,
               theme_dendro = FALSE) +  
  labs(y = "Dissimilarity between clusters") +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid = element_blank())

wwc_shots %>% mutate(Result=ifelse(shot.outcome.name=="Goal", 'Goal','No Goal'), Shots=n())%>% 
  filter(avevelocity<=50) %>%
  mutate(cluster=factor(cutree(wwc_complete, h=4))) %>% 
  ggplot(aes(DistSGK, avevelocity, color=cluster)) + 
  geom_point()+
  ggthemes::scale_color_colorblind()+
  facet_wrap(~shot.outcome.name)


wwc_shots %>% mutate(Result=ifelse(shot.outcome.name=="Goal", 'Goal','No Goal'), Shots=n())%>% 
  filter(avevelocity<=50) %>%
  mutate(cluster=factor(cutree(wwc_complete, h=4))) %>%  
  ggplot(aes(DistSGK, avevelocity, color=cluster)) + 
  geom_point()+
  ggthemes::scale_color_colorblind()+
  facet_wrap(~Result)


library(sportyR)
wnba_court <- geom_basketball("wnba", display_range = "offense", rotation = 270, x_trans = -41.5)
wnba_court +
  geom_hex(data = clark_shots, aes(x = shot_x, y = shot_y), binwidth = c(1, 1)) + 
  scale_fill_gradient(low = "midnightblue", high = "gold")


wwc_pitch <- geom_soccer("fifa", display_range = "offense", rotation = 90, x_trans = -41.5)


