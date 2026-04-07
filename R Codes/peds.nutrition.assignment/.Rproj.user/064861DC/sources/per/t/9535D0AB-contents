# Assessing Clustering Tendency.R
#
#
# Version:  1.1
#
# Date:     2020  Jan
# Author:   Paraskevi Massara (p.massara@utoronto.ca)
#
# Versions:
#   
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. 
#
# ==============================================================================

#== Objectives ==================================================================

# We will start by describing why clustering tendency is necessary before applying 
# any clustering method on a data. Next, we will apply visual and statistical 
# visual methods for assessing the clustering tendency.

# Please cite: :Massara P, Keown-Stoneman CD, Erdman L, Ohuma EO, Bourdon C, Maguire JL,
# Comelli EM, Birken C, Bandsma RH. Identifying longitudinal-growth patterns from infancy 
# to childhood: a study comparing multiple clustering techniques. Int J Epidemiol. 2021

# zBMI Growth Clusters
# Clustering children by zBMI at 24 months

# Research Question:
#   Are maternal dietary, socioeconomic, clinical factors, and gut microbiome 
#   associated with early childhood growth trajectories based on BMI-for-age z scores (zBMI)?
#
# Variables used for clustering: zBMI_24m 
#
# Data source: final_clean_data (post data cleaning to address implausible and missing values, 
# including imputation and column rearrangement)

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                          39
#TOC>   2        Data preparation                                  52      
#TOC>   3        Visual inspection                                 75
#TOC>   4        Why assessing clustering tendency is important?   91                
#TOC>
#TOC> ===========================================================================

# =    1  Packages  =============================================================

# Install and load required packages
packages <- c("NbClust", "factoextra", "ggplot2", "gridExtra", "cluster", 
              "RColorBrewer", "reshape2")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ============================================================================
# GENERATE SYNTHETIC DATA FOR TUTORIAL
# ============================================================================

set.seed(42)

# Create 3 well-separated clusters for demonstration
n_samples <- 150

# Cluster 1: centered at (2, 2)
cluster1 <- data.frame(
  X1 = rnorm(50, mean = 2, sd = 0.5),
  X2 = rnorm(50, mean = 2, sd = 0.5),
  X3 = rnorm(50, mean = 2, sd = 0.5),
  X4 = rnorm(50, mean = 2, sd = 0.5)
)

# Cluster 2: centered at (6, 6)
cluster2 <- data.frame(
  X1 = rnorm(50, mean = 6, sd = 0.5),
  X2 = rnorm(50, mean = 6, sd = 0.5),
  X3 = rnorm(50, mean = 6, sd = 0.5),
  X4 = rnorm(50, mean = 6, sd = 0.5)
)

# Cluster 3: centered at (4, 8)
cluster3 <- data.frame(
  X1 = rnorm(50, mean = 4, sd = 0.5),
  X2 = rnorm(50, mean = 8, sd = 0.5),
  X3 = rnorm(50, mean = 4, sd = 0.5),
  X4 = rnorm(50, mean = 8, sd = 0.5)
)

tutorial_data <- rbind(cluster1, cluster2, cluster3)
true_labels <- rep(1:3, each = 50)

# Scale the data
tutorial_data_scaled <- scale(tutorial_data)

# ============================================================
# GENERATE DATA PREPARATION FOR CLUSTERING
# ============================================================
# Research Question:
#   Are maternal dietary, socioeconomic, clinical factors, and gut microbiome associated 
#   with early childhood growth trajectories based on BMI-for-age z scores (zBMI)?

#1. Load data 
library(readr)
final_clean_data <- read_csv("final_clean_data.csv")
View(final_clean_data)

data <- final_clean_data

#2. Select and scale zBMI_24m 
clust_data <- data[!is.na(data$zBMI_24m), "zBMI_24m", drop = FALSE]
clust_scaled <- scale(clust_data)

# ============================================================================
# RUN NBCLUST ANALYSIS
# ============================================================================
cat("Running NbClust analysis\n")
cat("Testing cluster numbers from 2 to 8\n\n")

library(NbClust)

# Remove outliers before clustering (values beyond 3 SD inflate cluster count)
z_scores <- scale(clust_scaled)
clust_scaled_clean <- clust_scaled[abs(z_scores) <= 3, , drop = FALSE]
cat(sprintf("Rows after outlier removal: %d\n", nrow(clust_scaled_clean)))

set.seed(42)
nbclust_ts <- NbClust(
  data     = clust_scaled,
  distance = "euclidean",
  min.nc   = 2,
  max.nc   = 8,
  method   = "kmeans",
  index    = "all"
)

# Extract optimal k
votes <- nbclust_ts$Best.nc[1,]
votes <- votes[votes != 0]  # remove non-applicable indices
optimal_k <- as.numeric(names(which.max(table(votes))))
cat(sprintf("\nOptimal number of clusters: %d\n", optimal_k))

# ============================================================================
# PLOT 1: VOTING RESULTS
# ============================================================================
votes <- table(nbclust_ts$Best.nc[1,])
votes <- votes[votes != 0] 
vote_df <- as.data.frame(votes)
names(vote_df) <- c("k", "Votes")

# Round k to integer before any filtering to eliminate float duplicates
vote_df$k <- as.integer(round(as.numeric(as.character(vote_df$k))))

# Remove k < 2 and k > 8, then aggregate to collapse any remaining duplicates
vote_df <- vote_df[vote_df$k >= 2 & vote_df$k <= 8, ]
vote_df <- aggregate(Votes ~ k, data = vote_df, FUN = sum)

# Create highlight column AFTER aggregation
vote_df$highlight <- vote_df$k == optimal_k

plot1 <- ggplot(vote_df, aes(x = k, y = Votes, fill = k == optimal_k)) +
  geom_col(color = "black", linewidth = 0.8, width = 0.7) +  
  geom_text(aes(label = Votes), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "grey70"), guide = "none") +
  scale_x_continuous(breaks = vote_df$k) +
  labs(
    title = "A. NbClust Voting Results",
    subtitle = sprintf("Optimal k = %d (most votes)", optimal_k),
    x = "Number of Clusters (k)",
    y = "Number of Indices"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  )
print(plot1)
#===========================================================================
# PLOT 2: ELBOW METHOD
# ============================================================================

#To calculate the Within-Cluster Sum of Squares (WSS) for different numbers of clusters (k), 
#which is used to create an "Elbow Plot" to help choose the optimal number of clusters for K-Means.
wss <- sapply(1:8, function(k) {
  if (k == 1) {
    sum(scale(clust_scaled, scale = FALSE)^2)
  } else {
    kmeans(clust_scaled, centers = k, nstart = 25)$tot.withinss
  }
})

elbow_df <- data.frame(k = 1:8, WSS = wss)

plot2 <- ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line(color = "#3498DB", linewidth = 1.5) +
  geom_point(color = "#3498DB", size = 4) +
  geom_point(data = elbow_df[elbow_df$k == optimal_k, ], 
             aes(x = k, y = WSS), color = "#E74C3C", size = 6) +
  annotate("text", x = optimal_k, y = max(wss) * 0.85, 
           label = sprintf("Elbow at k = %d", optimal_k), 
           color = "#E74C3C", fontface = "bold", size = 5.5) +
  annotate("curve", x = optimal_k + 0.3, y = max(wss) * 0.83, 
           xend = optimal_k + 0.05, yend = wss[optimal_k] + 5,
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
           color = "#E74C3C", linewidth = 1) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "B. Elbow Method",
    subtitle = "Look for the bend (elbow) in the curve",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11, face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  )
plot2

# ============================================================================
# PLOT 3: SILHOUETTE ANALYSIS
# ============================================================================
#Runs k-means clustering on your scaled data
#Evaluates how good a k-means clustering solution is using the silhouette method
km_optimal <- kmeans(clust_scaled, centers = optimal_k, nstart = 25)
sil <- silhouette(km_optimal$cluster, dist(clust_scaled))
avg_sil_width <- mean(sil[, 3])

plot3 <- fviz_silhouette(sil, print.summary = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = sprintf("C. Silhouette Analysis (k = %d)", optimal_k),
    subtitle = sprintf("Average silhouette width = %.3f (higher is better)", avg_sil_width),
    x = "Observations",
    y = "Silhouette Width"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 13),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("tutorial_plot3_silhouette.png", plot3, width = 11, height = 7, dpi = 300, bg = "white")
ggsave("tutorial_plot3_silhouette.svg", plot3, width = 11, height = 7, bg = "white")
plot3


# Silhouette Analysis: works with any distance metric, detects cluster separation,
# and cohesion 
# ============================================================================
# PLOT 4: PCA CLUSTER VISUALIZATION
# ============================================================================
# PCA can not be run since we only have 1 variable (zBMI_24m)

# ============================================================================
# PLOT 5: COMPARISON OF MULTIPLE K VALUES
# ============================================================================

k_values <- 2:6
comparison_plots <- list()

for (k in k_values) {
  km_temp <- kmeans(clust_scaled, centers = k, nstart = 25)
  temp_df <- data.frame(
    zBMI_24m = data[!is.na(data$zBMI_24m), "zBMI_24m"],
    Cluster  = as.factor(km_temp$cluster)
  )
  p <- ggplot(temp_df, aes(x = zBMI_24m, fill = Cluster)) +
    geom_density(alpha = 0.5) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = sprintf("k = %d", k), x = "zBMI", y = "Density") +
    theme_classic(base_size = 14) +
    theme(
      plot.title       = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.title       = element_text(face = "bold", size = 12),
      legend.position  = "none",
      panel.grid.major = element_line(color = "grey95")
    )
  comparison_plots[[length(comparison_plots) + 1]] <- p
}

plot5 <- grid.arrange(
  grobs = comparison_plots,
  ncol  = 3,
  top   = grid::textGrob(
    "E. Comparing Different Numbers of Clusters",
    gp = grid::gpar(fontsize = 20, fontface = "bold")
  )
)
# Good separation in three clusters which supports k = 3 

# ============================================================================
# PLOT 6: INDEX HEATMAP
# ============================================================================

# Create matrix showing which k each index voted for
index_votes <- nbclust_result$Best.nc[1, ]
index_votes <- index_votes[!is.na(index_votes)]

index_matrix <- data.frame(
  Index = names(index_votes),
  Optimal_k = as.numeric(index_votes)
)

# Create categorical heatmap
index_matrix$Index_num <- 1:nrow(index_matrix)


index_votes <- nbclust_result$Best.nc[1, ]
index_votes <- index_votes[!is.na(index_votes) & index_votes >= 2]
index_matrix <- data.frame(
  Index     = names(index_votes),
  Optimal_k = as.numeric(index_votes)
)

index_matrix$Index_num <- seq_len(nrow(index_matrix))

plot6 <- ggplot(index_matrix, aes(x = Optimal_k, y = reorder(Index, Index_num))) +
  geom_tile(aes(fill = as.factor(Optimal_k)), color = "white", linewidth = 1) +
  geom_text(aes(label = Optimal_k), color = "white", fontface = "bold", size = 4) +
  scale_fill_brewer(palette = "Spectral", name = "Optimal k") +
  scale_x_continuous(breaks = 2:8, expand = c(0, 0)) +
  labs(
    title = "F. Individual Index Recommendations",
    subtitle = "Each row shows one index's vote for optimal k",
    x = "Recommended Number of Clusters",
    y = "Statistical Index"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "grey30", hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

plot6
# ============================================================================
# CREATE COMBINED FIGURE
# ============================================================================

combined <- grid.arrange(
  plot1, plot2, plot3, plot6,
  ncol = 2, nrow = 2,
  top = grid::textGrob(
    "NbClust Tutorial: Complete Analysis",
    gp = grid::gpar(fontsize = 22, fontface = "bold"),
    vjust = 1
  )
)
ggsave("Complete Analysis of Clustering Plots.png", combined,
       width = 16, height = 12, dpi = 300, bg = "white")


# ============================================================================
# GENERATE SUMMARY REPORT
# ============================================================================

sink("tutorial_nbclust_summary.txt")

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("NbClust TUTORIAL - ANALYSIS SUMMARY\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("DATASET INFORMATION:\n")
cat(sprintf("  • Number of observations: %d\n", nrow(clust_scaled)))
cat(sprintf("  • Number of features: %d\n", ncol(clust_scaled)))
cat(sprintf("  • True number of clusters: 3\n\n"))

cat("NBCLUST RECOMMENDATION:\n")
cat(sprintf("  • Optimal number of clusters: %d\n", optimal_k))
cat(sprintf("  • Number of indices tested: %d\n", length(index_votes)))
cat("\n")

cat("VOTING BREAKDOWN:\n")
for (i in 1:nrow(vote_df)) {
  cat(sprintf("  • k = %d: %d votes\n", vote_df$k[i], vote_df$Votes[i]))
}
cat("\n")

cat("CLUSTER QUALITY METRICS:\n")
cat(sprintf("  • Average Silhouette Width: %.3f\n", avg_sil_width))
cat(sprintf("  • Total Within-Cluster SS: %.2f\n", km_optimal$tot.withinss))
cat(sprintf("  • Between-Cluster SS / Total SS: %.1f%%\n", 
            100 * km_optimal$betweenss / km_optimal$totss))
cat("\n")

cat("INTERPRETATION GUIDE:\n")
cat("  • Silhouette width > 0.50: Good cluster structure\n")
cat("  • Silhouette width 0.25-0.50: Weak cluster structure\n")
cat("  • Silhouette width < 0.25: No substantial cluster structure\n\n")

cat("FILES GENERATED:\n")
cat("  1. tutorial_plot1_voting.png/svg - Voting results\n")
cat("  2. tutorial_plot2_elbow.png/svg - Elbow method\n")
cat("  3. tutorial_plot3_silhouette.png/svg - Silhouette analysis\n")
cat("  4. tutorial_plot4_pca.png/svg - PCA visualization\n")
cat("  5. tutorial_plot5_comparison.png/svg - Multiple k comparison\n")
cat("  6. tutorial_plot6_index_heatmap.png/svg - Index recommendations\n")
cat("  7. tutorial_combined_all.png/pdf - Combined figure\n")
cat("  8. tutorial_nbclust_summary.txt - This summary\n\n")

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")

sink()



cat("\n", strrep("=", 70), "\n")
cat("CLUSTERING TENDENCY ANALYSIS — SUMMARY\n")
cat(strrep("=", 70), "\n\n")
cat(sprintf("Observations: %d\n", n_obs))
cat(sprintf("Clustering variables: %d\n", length(clust_vars)))
cat(sprintf("NbClust majority vote: k = %d\n", optimal_k))
cat(sprintf("Silhouette-optimal k:  k = %d\n", final_k))
cat(sprintf("Average silhouette width (final): %.4f\n", avg_sil))
cat(sprintf("Target met (>= 0.51): %s\n\n", ifelse(avg_sil >= 0.51, "YES ✓", "CLOSE — see note")))
cat("Interpretation:\n")
cat("  > 0.70  Strong structure\n")
cat("  0.51–0.70  Reasonable structure\n")
cat("  0.26–0.50  Weak structure\n")
cat("  < 0.25  No substantial structure\n\n")
cat(strrep("=", 70), "\n")

# Define helper for pipe
`%>%` <- function(x, f) f(x)