# clustering_with_pca_and_2var_kmeans.R

# I make sure the data is loaded once
if (!exists("wwc_shots")) {
  wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_shots.csv")
}

# I load the packages I need
library(tidyverse)    # for data wrangling & ggplot2
library(cluster)      # for silhouette analysis
library(factoextra)   # for silhouette helpers and PCA biplot

# Part 1: PCA biplot of all shot features

# I pick my full feature set and drop any rows with missing values
full_features <- wwc_shots %>%
  select(duration, DistToGoal, AngleToGoal, DefendersInCone, avevelocity) %>%
  drop_na()

# I scale the features so they’re comparable
scaled_full <- scale(full_features)

# I check silhouette widths to choose how many clusters I might want later
fviz_nbclust(scaled_full, kmeans, method = "silhouette") +
  labs(
    title = "Silhouette Method: Avg. Silhouette Width vs. Number of Clusters",
    x     = "Number of Clusters",
    y     = "Avg. Silhouette Width"
  )

# I run PCA so I can see the shot data in two dimensions
pca_res <- prcomp(scaled_full, center = TRUE, scale. = FALSE)

# I draw a PCA biplot to inspect both shots (points) and feature loadings (arrows)
fviz_pca_biplot(pca_res,
                geom.ind   = "point",
                pointsize  = 2,
                repel      = TRUE) +
  labs(title = "PCA Biplot: WWC Shot Features")


# Part 2: K-means on the two most informative variables

# I choose duration and distance as the two key variables for clustering
two_vars <- wwc_shots %>%
  select(duration, DistToGoal) %>%
  drop_na()

# I scale them for fair comparison
scaled_2d <- scale(two_vars)

# I use silhouette again to confirm how many groups to cut into
fviz_nbclust(scaled_2d, kmeans, method = "silhouette") +
  labs(
    title = "Silhouette (2D): Avg. Width vs. Number of Clusters",
    x     = "Number of Clusters",
    y     = "Avg. Silhouette Width"
  )

# I decide on 3 clusters for duration vs distance
k_2d <- 3

# I run k-means with a robust nstart
set.seed(123)
km_2d <- kmeans(scaled_2d, centers = k_2d, nstart = 25)

# I attach the cluster labels back to the original two-variable data
clustered_2d <- two_vars %>%
  mutate(cluster = factor(km_2d$cluster))

# I compute the real‐scale centroids for plotting
centroids_2d <- clustered_2d %>%
  group_by(cluster) %>%
  summarise(
    duration   = mean(duration),
    DistToGoal = mean(DistToGoal),
    .groups    = "drop"
  )

# I plot duration vs. distance, coloring by cluster and marking centroids
ggplot(clustered_2d, aes(x = duration, y = DistToGoal, color = cluster)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_point(data = centroids_2d,
             aes(x = duration, y = DistToGoal),
             shape = 8, size = 4, color = "black") +
  labs(
    title = paste("K-Means Clustering on Duration & Distance (k =", k_2d, ")"),
    x     = "Shot Duration (s)",
    y     = "Distance to Goal (m)",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# clustering_timepos_dist.R

# I load the libraries I need
library(tidyverse)    # for data wrangling & ggplot2
library(cluster)      # for silhouette analysis
library(factoextra)   # for silhouette helper

# I pick possession time and shot distance and drop missing values
data_td <- wwc_shots %>%
  select(TimeInPoss, DistToGoal) %>%
  drop_na()

# I scale those two variables so they’re comparable
scaled_td <- scale(data_td)

# I use silhouette to decide on how many clusters make sense
fviz_nbclust(scaled_td, kmeans, method = "silhouette") +
  labs(
    title = "Silhouette: Avg. Width vs. Number of Clusters",
    x     = "Number of Clusters",
    y     = "Average Silhouette Width"
  )

# I pick k = 3 based on that plot
k_td <- 3

# I run k-means clustering with multiple starts for stability
set.seed(2025)
km_td <- kmeans(scaled_td, centers = k_td, nstart = 25)

# I attach the cluster labels back to the original data
clustered_td <- data_td %>%
  mutate(cluster = factor(km_td$cluster))

# I compute centroids in the original scale for plotting
centroids_td <- clustered_td %>%
  group_by(cluster) %>%
  summarise(
    TimeInPoss = mean(TimeInPoss),
    DistToGoal = mean(DistToGoal),
    .groups    = "drop"
  )

# I plot possession time vs. shot distance, coloring by cluster and marking centroids
ggplot(clustered_td, aes(x = TimeInPoss, y = DistToGoal, color = cluster)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_point(data = centroids_td,
             aes(x = TimeInPoss, y = DistToGoal),
             shape = 8, size = 4, color = "black") +
  labs(
    title = paste("K-Means on Possession Time & Distance (k =", k_td, ")"),
    x     = "Time in Possession (s)",
    y     = "Distance to Goal (m)",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))
