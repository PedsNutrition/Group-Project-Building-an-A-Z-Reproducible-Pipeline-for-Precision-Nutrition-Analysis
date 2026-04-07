# Assessing Clustering Tendency.R
#
#
# Version:  2
#
# Date:  2026  Apr
# Authors: Yue-Tong Chen, Brinley Klievik, Komang Triyani, Eva Kranenburg
#
# Original Author:   Paraskevi Massara (p.massara@utoronto.ca)
#
# Versions:
#   
# Relevant citation: :Massara P, Keown-Stoneman CD, Erdman L, Ohuma EO, Bourdon C, Maguire JL,
# Comelli EM, Birken C, Bandsma RH. Identifying longitudinal-growth patterns from infancy 
# to childhood: a study comparing multiple clustering techniques. Int J Epidemiol. 2021

#TOC> ====================================================================2======
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                          28
#TOC    2        Histogram of Raw zBMI Scores                      58
#TOC>   3        NbClust Analysis                                 100      
#TOC>   4        Visualizing Clustering                           120            
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

library(dplyr)

# Load cleaned data set used for analysis

clean_data <- read.csv("final_clean_data.csv")

temp <- dplyr::select(clean_data, zBMI_24m)
head(temp)

data <-  tidyr::drop_na(temp)

cat("Observations available for clustering:", nrow(data), "\n")
cat("Observations excluded due to missing zBMI_24m:", nrow(clean_data)- nrow(data), "\n")

# Scale the data
data_scaled <- scale(data)  # Scaling applied for consistency

# =  2   Histogram of Raw zBMI_24m Distributions  ====================================

# Shows overall distribution before clustering, with normal curve overlay
# Used to assess heterogeneity and modality of the data

hist_df <- data.frame(zBMI_24m = data$zBMI_24m)

plot0 <- ggplot(hist_df, aes(x = zBMI_24m)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#3498DB", color = "white", alpha = 0.8) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(hist_df$zBMI_24m), sd = sd(hist_df$zBMI_24m)),
    color = "#E74C3C", linewidth = 1.2, linetype = "dashed"
  ) +
  annotate("text",
           x = max(hist_df$zBMI_24m) * 0.75,
           y = max(density(hist_df$zBMI_24m)$y) * 0.95,
           label = sprintf("n = %d\nMean = %.2f\nSD = %.2f",
                           nrow(hist_df),
                           mean(hist_df$zBMI_24m),
                           sd(hist_df$zBMI_24m)),
           size = 5, hjust = 0, color = "grey20") +
  labs(
    title = "zBMI at 24 Months — Overall Distribution",
    subtitle = "Histogram with normal curve overlay (dashed red line)",
    x = "zBMI at 24 months",
    y = "Density"
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

ggsave("zBMI_24m_plot0_histogram.png", plot0, width = 10, height = 7, dpi = 300, bg = "white")
plot0

# ===== 3   NBClust analysis to determine optimal number of clusters =========================

cat("Running NbClust analysis for zBMI 24m...\n")
cat("Testing cluster numbers from 2 to 8...\n\n")

nbclust_result <- NbClust(
  data = data_scaled,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 8,
  method = "kmeans",
  index = "all"
)

# Extract optimal k
votes <- nbclust_result$Best.nc[1,]
votes <- votes[votes != 0]  # remove non-applicable indices
optimal_k <- as.numeric(names(which.max(table(votes))))
cat(sprintf("\nOptimal number of clusters: %d\n", optimal_k))

# ====== 4    Visualizing Clustering ======================================================

# ============================================================================
# PLOT 1: VOTING RESULTS
# ============================================================================

vote_df <- as.data.frame(table(votes))
names(vote_df) <- c("k", "Votes")
vote_df$k <- as.numeric(as.character(vote_df$k))

plot1 <- ggplot(vote_df, aes(x = k, y = Votes, fill = k == optimal_k)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.8) +
  geom_text(aes(label = Votes), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("grey70", "#E74C3C"), guide = "none") +
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
ggsave("zBMI_24m_plot1_votes.png", plot1, width = 10, height = 7, dpi = 300, bg = "white")
plot1

#===========================================================================
# PLOT 2: ELBOW METHOD
# ============================================================================

wss <- sapply(1:8, function(k) {
  if (k == 1) {
    sum(scale(data_scaled, scale = FALSE)^2)
  } else {
    kmeans(data_scaled, centers = k, nstart = 25)$tot.withinss
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
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  )
ggsave("zBMI_24m_plot2_elbow.png", plot2, width = 10, height = 7, dpi = 300, bg = "white")
plot2

# ============================================================================
# PLOT 3: SILHOUETTE ANALYSIS
# ============================================================================

km_optimal <- kmeans(data_scaled, centers = optimal_k, nstart = 25)
sil <- silhouette(km_optimal$cluster, dist(data_scaled))
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

ggsave("zBMI_24m_plot3_silhouette.png", plot3, width = 11, height = 7, dpi = 300, bg = "white")
plot3

# Silhouette Analysis: works with any distance metric, detects cluster separation,
# and cohesion 
# ============================================================================
# PLOT 4: CLUSTER DENSITY PLOT
# ============================================================================

plot4_df <- data.frame(
  zBMI_24m = data_scaled[, 1],
  Cluster = as.factor(km_optimal$cluster)
)

plot4 <- ggplot(plot4_df, aes(x = zBMI_24m, fill = Cluster)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = sprintf("D. Cluster Distributions (k = %d)", optimal_k),
    x = "zBMI at 24 months (scaled)",
    y = "Density"
  ) +
  theme_classic(base_size = 16)
ggsave("zBMI_24m_plot4_density.png", plot4, width = 10, height = 7, dpi = 300, bg = "white")
plot4
# ============================================================================
# PLOT 5: COMPARISON OF MULTIPLE K VALUES
# ============================================================================

k_values <- 2:6
comparison_plots <- list()

for (k in k_values) {
  km_temp <- kmeans(data_scaled, centers = k, nstart = 25)
  temp_df <- data.frame(
    zBMI_24m = data_scaled[, 1],
    Cluster = as.factor(km_temp$cluster)
  )
  
  p <- ggplot(temp_df, aes(x = zBMI_24m, fill = Cluster, color = Cluster)) +
    geom_density(alpha = 0.4, linewidth = 1) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2") +
    labs(title = sprintf("k = %d", k),
         x = "zBMI 24m (scaled)",
         y = "Density") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      legend.position = "none",
      panel.grid.major = element_line(color = "grey95")
    )
  
  comparison_plots[[length(comparison_plots) + 1]] <- p
}

plot5 <- grid.arrange(
  grobs = comparison_plots,
  ncol = 3,
  top = grid::textGrob(
    "E. Comparing Different Numbers of Clusters",
    gp = grid::gpar(fontsize = 20, fontface = "bold")
  )
)
ggsave("zBMI_24m_plot5_kmeans_comparison.png", plot5, width = 10, height = 7, dpi = 300, bg = "white")

# ============================================================================
# PLOT 6: INDEX HEATMAP
# ============================================================================

# Create matrix showing which k each index voted for
index_votes <- nbclust_result$Best.nc[1, ]
index_votes <- index_votes[!is.na(index_votes) & index_votes != 0]

index_matrix <- data.frame(
  Index = names(index_votes),
  Optimal_k = as.numeric(index_votes)
)

# Create categorical heatmap
index_matrix$Index_num <- 1:nrow(index_matrix)

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
ggsave("zBMI_24m_plot6_heatmap.png", plot6, width = 10, height = 7, dpi = 300, bg = "white")
plot6
# ============================================================================
# CREATE COMBINED FIGURE
# ============================================================================

combined <- grid.arrange(
  plot1, plot2, plot3, plot4,
  ncol = 2, nrow = 2,
  top = grid::textGrob(
    "NbClust: Complete Analysis",
    gp = grid::gpar(fontsize = 22, fontface = "bold"),
    vjust = 1
  )
)

sink()
