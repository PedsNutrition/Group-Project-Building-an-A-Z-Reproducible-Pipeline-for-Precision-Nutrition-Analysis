# ==========================================================================
# DATA PREPROCESSING R PACKAGE 1
# ==========================================================================
# ===== 1  Packages =======================================================

packages <- c("tidyverse", "naniar", "skimr", "ggplot2", "dplyr")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ===== 2  Data loading ========================================================

data <- read_csv("C:/Users/iiiam/OneDrive/Desktop/PhD/Nutritional Sciences/Comelli/Courses/NFS1218/Group Project/Group project all/mock_precision_growth_dataset.csv")

# Inspect structure
glimpse(data)
summary(data)

# ===== 3  Missingness exploration ==========================================

# 3.1 Percent missing per variable
missing_summary <- data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary)

# 3.2 Visual inspection of missing data
vis_miss(data) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# Missingness combinations
gg_miss_upset(data)

# 3.3 Missingness by subgroup (Sex)

if ("Sex" %in% names(data)) {
  
  data %>%
    group_by(Sex) %>%
    summarise(across(everything(),
                     ~ mean(is.na(.)) * 100))
}

# ===== 4  Distribution inspection ========================================

# 4.1 zBMI distribution
if ("zBMI_24m" %in% names(data)) {
  print(
    ggplot(data, aes(x = zBMI_24m)) +
      geom_histogram(bins = 30) +
      theme_minimal() +
      labs(title = "Distribution of zBMI at 24 months")
  )
}

# 4.2 CRP distribution

if ("CRP" %in% names(data)) {
  
  print(
    ggplot(data, aes(x = CRP)) +
      geom_histogram(bins = 30) +
      theme_minimal() +
      labs(title = "Distribution of CRP")
  )
  
  # Replace negative CRP with NA before log-transforming
  data$CRP[data$CRP < 0] <- NA
  
  # Log-transform CRP
  # log1p = log(CRP + 1) to handle zero values
  data$log_CRP <- log1p(data$CRP)
  
  print(
    ggplot(data, aes(x = log_CRP)) +
      geom_histogram(bins = 30) +
      theme_minimal() +
      labs(title = "Distribution of log(CRP)")
  )
}

# ===== 5  Identifying implausible values ==================================

# 5.1 Implausible age values
if ("Age_24m_months" %in% names(data)) {
  implausible_age <- dplyr::filter(data, Age_24m_months < 0 | Age_24m_months > 60)
  cat("Implausible age values:", nrow(implausible_age), "\n")
  print(implausible_age)
}

# 5.2 WHO-style plausibility cut-offs for zBMI
# WHO commonly flags z-scores < -5 or > +5 as implausible
if ("zBMI_24m" %in% names(data)) {
  implausible_zbmi <- dplyr::filter(data, zBMI_24m < -5 | zBMI_24m > 5)
  cat("Implausible zBMI values:", nrow(implausible_zbmi), "\n")
  print(implausible_zbmi)
}

# 5.3 Extreme CRP values (possible acute infection)
if ("CRP" %in% names(data)) {
  extreme_crp <- dplyr::filter(data, CRP > 10)
  cat("Extreme CRP values:", nrow(extreme_crp), "\n")
  print(extreme_crp)
}

# ===== 6  Data cleaning ===================================================

clean_data <- data

# Remove implausible age
if ("Age_24m_months" %in% names(clean_data)) {
  clean_data <- dplyr::filter(clean_data, Age_24m_months >= 0 & Age_24m_months <= 60)
}

# Remove implausible zBMI
if ("zBMI_24m" %in% names(clean_data)) {
  clean_data <- dplyr::filter(clean_data, zBMI_24m >= -5 & zBMI_24m <= 5)
}

# Remove extreme CRP > 10 mg/L
if ("CRP" %in% names(clean_data)) {
  clean_data <- dplyr::filter(clean_data, CRP <= 10 | is.na(CRP))
}

# ===== 7  Post-cleaning diagnostics =======================================

# Compare sample size
cat("Original N:", nrow(data), "\n")
cat("Cleaned N:", nrow(clean_data), "\n")

# Recalculate missingness after cleaning
missing_summary_clean <- clean_data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary_clean)

# Save cleaned dataset
write_csv(clean_data, "C:/Users/iiiam/OneDrive/Desktop/PhD/Nutritional Sciences/Comelli/Courses/NFS1218/Group Project/Group project all/final_dataset_april.csv")
cat("Cleaned dataset saved as final_dataset_april.csv\n")

# ==========================================================================
# ASSESSING CLUSTERING TENDENCY R PACKAGE 2
# ==========================================================================
# ===== 1  Packages =============================================================
packages <- c("NbClust", "factoextra", "ggplot2", "gridExtra", "cluster", 
              "RColorBrewer", "reshape2")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ===== 2  Data preparation ====================================================

# Load cleaned dataset
data <- read.csv("final_dataset_april.csv")

# Select zBMI_24m for clustering
df <- data[!is.na(data$zBMI_24m), "zBMI_24m", drop = FALSE]

# Scale the data
df.scaled <- scale(df)

# View the first 3 rows
head(df.scaled, n = 3)

# ===== 3  NBClust analysis to determine optimal number of clusters ====================================================
cat("Running NbClust analysis for tutorial...\n")
cat("Testing cluster numbers from 2 to 8...\n\n")

nbclust_result <- NbClust(
  data = df.scaled,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 8,
  method = "kmeans",
  index = "all"
)

# Extract optimal k
optimal_k <- as.numeric(names(which.max(table(nbclust_result$Best.nc[1,]))))
cat(sprintf("\nOptimal number of clusters: %d\n", optimal_k))

optimal_k <- 3

# ============================================================================
# PLOT 1: VOTING RESULTS
# ============================================================================

votes <- table(nbclust_result$Best.nc[1,])
vote_df <- as.data.frame(votes)
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
print(plot1)

# ===========================================================================
# PLOT 2: ELBOW METHOD
# ============================================================================

wss <- sapply(1:8, function(k) {
  if (k == 1) {
    sum(scale(df.scaled, scale = FALSE)^2)
  } else {
    kmeans(df.scaled, centers = k, nstart = 25)$tot.withinss
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
print(plot2)

# ============================================================================
# PLOT 3: SILHOUETTE ANALYSIS
# ============================================================================
km_optimal <- kmeans(df.scaled, centers = optimal_k, nstart = 25)
sil <- silhouette(km_optimal$cluster, dist(df.scaled))
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
print(plot3)

# Silhouette Analysis: works with any distance metric, detects cluster separation,
# and cohesion 

# ============================================================================
# PLOT 4: PCA CLUSTER VISUALIZATION
# ============================================================================
# Note that PCA can not be run since we only have 1 variable (zBMI_24m)

# ============================================================================
# PLOT 5: COMPARISON OF MULTIPLE K VALUES
# ============================================================================

k_values <- 2:6
comparison_plots <- list()

for (k in k_values) {
  km_temp <- kmeans(df.scaled, centers = k, nstart = 25)
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

# ============================================================================
# PLOT 6: INDEX HEATMAP
# ============================================================================

index_votes <- nbclust_result$Best.nc[1, ]
index_votes <- index_votes[!is.na(index_votes)]

index_matrix <- data.frame(
  Index     = names(index_votes),
  Optimal_k = as.numeric(index_votes)
)
index_matrix$Index_num <- 1:nrow(index_matrix)

plot6 <- ggplot(index_matrix, aes(x = Optimal_k, y = reorder(Index, Index_num))) +
  geom_tile(aes(fill = as.factor(Optimal_k)), color = "white", linewidth = 1) +
  geom_text(aes(label = Optimal_k), color = "white", fontface = "bold", size = 4) +
  scale_fill_brewer(palette = "Spectral", name = "Optimal k") +
  scale_x_continuous(breaks = 2:8, expand = c(0, 0)) +
  labs(
    title    = "F. Individual Index Recommendations",
    subtitle = "Each row shows one index's vote for optimal k",
    x        = "Recommended Number of Clusters",
    y        = "Statistical Index"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle   = element_text(size = 14, color = "grey30", hjust = 0.5, margin = margin(b = 10)),
    axis.title      = element_text(face = "bold", size = 16),
    axis.text.x     = element_text(size = 13, face = "bold"),
    axis.text.y     = element_text(size = 10),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 14),
    panel.grid      = element_blank(),
    plot.margin     = margin(20, 20, 20, 20)
  )
print(plot6)

# ============================================================================
# COMBINED FIGURE
# ============================================================================

combined <- grid.arrange(
  plot1, plot2, plot3, plot6,
  ncol = 2, nrow = 2,
  top  = grid::textGrob(
    "Determining Optimal Number of zBMI Growth Trajectory Clusters Summary",
    gp = grid::gpar(fontsize = 22, fontface = "bold"),
    vjust = 1
  )
)
print(combined)

# ==========================================================================
# DIST COMPUTATION R PACKAGE 3
# ==========================================================================
# ===== 1  Packages  =============================================================

if (! require(factoextra, quietly=TRUE)) {
  install.packages(factoextra)
  library(factoextra)
}

if (! require(hopkins, quietly=TRUE)) {
  install.packages(hopkins)
  library(hopkins)
}

# ===== 2  Data preparation  ====================================================

# Load cleaned dataset
data <- read.csv("final_dataset_april.csv")

# Select zBMI_24m and remove missing values
df_full <- data[!is.na(data$zBMI_24m), "zBMI_24m", drop = FALSE]

# ===== 3  Computing distances  ==================================================

# Using euclidean metric
dist.eucl <- dist(df, method = "euclidean")

# Using manhattan metric
dist.manh <- dist(df, method = "manhattan")

# Other metrics "binary", "minkowski"

# Let's visualize the distance matrices
raw_eucl_m <- fviz_dist(dist.eucl) +
  labs(title = "Euclidean Distance - Raw")
raw_manh_m <- fviz_dist(dist.manh) +
  labs(title = "Manhattan Distance - Raw")

print(raw_eucl_m)
print(raw_manh_m)

# ===== 4  Data standardization  =================================================

library(gridExtra)

# Standardize the data
df.scaled <- scale(df)

# Compute euclidean distance on standardized data
dist.eucl.std <- dist(df.scaled, method = "euclidean")
std_eucl_m <- fviz_dist(dist.eucl.std) +
  labs(title = "Euclidean Distance - Standardized")

grid.arrange(
  raw_eucl_m, std_eucl_m,
  ncol = 2,
  top  = "Euclidean Distance: Raw (left) vs Standardized (right)"
)

# ==========================================================================
# CLUSTERING R PACKAGE 4
# ==========================================================================
# ===== 1  Packages  =============================================================

if (!require(factoextra, quietly = TRUE)) {
  install.packages(factoextra)
  library(factoextra)
}

if (!require(clustertend, quietly = TRUE)) {
  install.packages(clustertend)
  library(clustertend)
}

if (!require(igraph, quietly = TRUE)) {
  install.packages(igraph)
  library(igraph)
}

if (!require(cluster, quietly = TRUE)) {
  install.packages(cluster)
  library(cluster)
}


if (!require(dendextend, quietly = TRUE)) {
  install.packages(dendextend)
  library(dendextend)
}

# ===== 2  Data preparation  ====================================================

# Load cleaned dataset
data <- read.csv("final_dataset_april.csv")

# Select zBMI_24m and remove missing values
df_full <- data[!is.na(data$zBMI_24m), "zBMI_24m", drop = FALSE]

# Scale the data
df <- scale(df_full)

# View the first 3 rows
head(df, n = 3)

# ===== 3  Clustering with k-means  =============================================
# Visualize k-means results — boxplot instead of fviz_cluster since we only have 1 variable (zBMI_24m)
km_df <- data.frame(
  zBMI_24m = df_full$zBMI_24m,
  Cluster  = as.factor(km.res$cluster)
)

print(
  ggplot(km_df, aes(x = Cluster, y = zBMI_24m, fill = Cluster)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("#2E9FDF", "#00AFBB", "#E7B800")[1:optimal_k]) +
    theme_minimal() +
    labs(
      title = paste("K-Means Clustering of zBMI_24m (k =", optimal_k, ")"),
      x     = "Cluster",
      y     = "zBMI at 24 Months"
    )
)

# ===== 4  Clustering with PAM  =================================================
# Visualize PAM results — boxplot instead of fviz_cluster since we only have 1 variable (zBMI_24m)
pam_df <- data.frame(
  zBMI_24m = df_full$zBMI_24m,
  Cluster  = as.factor(pam.res$clustering)
)

print(
  ggplot(pam_df, aes(x = Cluster, y = zBMI_24m, fill = Cluster)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")[1:optimal_k]) +
    theme_minimal() +
    labs(
      title = paste("PAM Clustering of zBMI_24m (k =", optimal_k, ")"),
      x     = "Cluster",
      y     = "zBMI at 24 Months"
    )
)

table(pam.res$clustering)

# =   5  Hierarchical clustering  ==============================================
# Compute the dissimilarity matrix
res.dist <- dist(df, method = "euclidean")

# Display first 6 rows and columns
as.matrix(res.dist)[1:6, 1:6]

# Hierarchical clustering
res.hc <- hclust(d = res.dist, method = "complete")

# Visualize the dendrogram
print(fviz_dend(res.hc, cex = 0.5))

# Cut tree into optimal_k groups
grp <- cutree(res.hc, k = optimal_k)
head(grp, n = 4)

# Number of members in each cluster
table(grp)

# Get the members of cluster 1
rownames(df)[grp == 1]

# Coloured dendrogram
print(
  fviz_dend(
    res.hc,
    k                 = optimal_k,
    cex               = 0.5,
    k_colors          = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")[1:optimal_k],
    color_labels_by_k = TRUE,
    rect              = TRUE
  )
)

# Circular dendrogram
print(
  fviz_dend(
    res.hc,
    cex      = 0.5,
    k        = optimal_k,
    k_colors = "jco",
    type     = "circular"
  )
)

# Phylogenic tree
print(
  fviz_dend(
    res.hc,
    k            = optimal_k,
    k_colors     = "jco",
    type         = "phylogenic",
    repel        = TRUE,
    phylo_layout = "layout.gem"
  )
)

# ==========================================================================
# CLUSTER VALIDATION R PACKAGE 5
# ==========================================================================
# ===== 1  Packages  =======================================================
if (! require(clValid, quietly=TRUE)) {
  install.packages(clValid)
  library(clValid)
}

if (! require(clustertend, quietly=TRUE)) {
  install.packages(clustertend)
  library(clustertend)
}

# ===== 2  Data preparation  ====================================================
# Load cleaned dataset and select zBMI_24m
data <- read.csv("final_dataset_april.csv")
df_full <- data[!is.na(data$zBMI_24m), "zBMI_24m", drop = FALSE]

# Scale the data
df <- scale(df_full)

# ===== 3  Internal validation  =================================================
# Compute clValid
clmethods <- c("hierarchical", "kmeans", "pam")

intern <- clValid(df,
                  nClust     = 2:6,
                  clMethods  = clmethods,
                  validation = "internal")

# Summary
summary(intern)

# ===== 4  Stability validation  ================================================
# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(df, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
# Note that stability test requires at least 2 variables, therefore we can not conduct it on zBMI_24m only.

# ===== 5  Visualize validation results  ========================================

# Internal validation plot
op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(intern, legend = FALSE)
par(op)

