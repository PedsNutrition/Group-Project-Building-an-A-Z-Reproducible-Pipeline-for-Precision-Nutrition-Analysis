# CLUSTERING.R
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

# We will identify clusters using k-means and hierarchical clustering.
# Next, we will visualize the obtained clusters and we will combine clustering with
# dimention reduction methods.

#RESOURCES:http://www.sthda.com/english/

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                          39
#TOC>   2        Data preparation                                  61
#TOC>   3        Clustering with K-means                           73
#TOC>   4        Clustering with PAM                               123
#TOC>   5        Hierarchical clustering                           172
#TOC> ===========================================================================

# =    1  Packages  =============================================================

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
# =    2  Data preparation  ====================================================

# Load cleaned dataset
data <- read.csv("final_clean_data.csv")

# Select zBMI_24m and remove missing values
df_full <- data[!is.na(data$zBMI_24m), "zBMI_24m", drop = FALSE]

# Scale the data
df <- scale(df_full)

# View the first 3 rows of the data
head(df, n = 3)


# =    3  Clustering with k-means  ====================================================

# kmeans(x, centers, iter.max = 10, nstart = 1)

# .x: numeric matrix, numeric data frame or a numeric vector
# . centers: Possible values are the number of clusters (k) or a set of initial (distinct)
# cluster centers. If a number, a random set of (distinct) rows in x is chosen as
# the initial centers.
# . iter.max: The maximum number of iterations allowed. Default value is 10.
# . nstart: The number of random starting partitions when centers is a number.
# Trying nstart > 1 is often recommended.

# The function requires to specify the number of clusters.
# How to choose the right number of expected clusters?

# We will conduct k-means clustering using different values of clusters k.
# Next, the wss (within sum of square) is drawn according
# to the number of clusters. The location of a bend (knee) in the plot is generally
# considered as an indicator of the appropriate number of clusters.
# Install if not already installed

install.packages("factoextra")
# Load the package
library(factoextra)

# Compute k-means with k = 3
set.seed(123)
km.res <- kmeans(df, 3, nstart = 25)

# Compute the mean of each variables of each cluster

aggregate(final_clean_data, by = list(cluster = km.res$cluster), mean)
dd <- cbind(final_clean_data, cluster = km.res$cluster)
head(dd)

# Visualize k-means results — boxplot instead of fviz_cluster since we only have 1 variable (zBMI_24m)
km_df <- data.frame(
  zBMI_24m = df_full$zBMI_24m,
  Cluster  = as.factor(km.res$cluster)
)

print(
  ggplot(km_df, aes(x = Cluster, y = zBMI_24m, fill = Cluster)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("red", "#00AFBB", "#E7B800")[1:optimal_k]) +
    theme_minimal() +
    labs(
      title = paste("K-Means Clustering of zBMI_24m (k =", optimal_k, ")"),
      x     = "Cluster",
      y     = "zBMI at 24 Months"
    )
)


# =    4  Clustering with PAM  ====================================================

# pam(x, k, metric = "euclidean", stand = FALSE)

# x: possible values includes:
#   - Numeric data matrix or numeric data frame: each row corresponds to an
# observation, and each column corresponds to a variable.
# - Dissimilarity matrix: in this case x is typically the output of daisy() or
# dist()
# . k: The number of clusters
# . metric: the distance metrics to be used. Available options are "euclidean" and
# "manhattan".
# . stand: logical value; if true, the variables (columns) in x are standardized before
# calculating the dissimilarities. Ignored when x is a dissimilarity matrix.

#Estimate the optimal number of clusters.

# Visualize PAM results — boxplot instead of fviz_cluster since we only have 1 variable (zBMI_24m)
library(cluster)

# Run PAM clustering 
pam.res <- pam(df_full, k = 3)

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
      title = paste("PAM Clustering of zBMI at 24months (k =", optimal_k, ")"),
      x     = "Cluster",
      y     = "zBMI at 24 Months"
    )
)

table(pam.res$clustering)

# =   5  Hierarchical clustering  ====================================================

# Compute the dissimilarity matrix
# df = the standardized data

res.dist <- dist(df, method = "euclidean")

as.matrix(res.dist)[1:6, 1:6]  #display the first 6 rows and columns of the distance matrix

res.hc <- hclust(d = res.dist, method = "complete")

# Visualize the results

fviz_dend(res.hc, cex = 0.5)# cex: label size

# Cut the dendrogram into different groups

# Hierarchical clustering does not tell us how many clusters there are,
# or where to cut the dendrogram to form clusters

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 3)
head(grp, n = 4)

# Number of members in each cluster
table(grp)

# Get the names for the members of cluster 1
rownames(df)[grp == 1]


fviz_dend(
  res.hc,
  k = 3,
  # Cut in four groups
  cex = 0.5,
  # label size
  k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
  color_labels_by_k = TRUE,
  # color labels by groups
  rect = TRUE # Add rectangle around groups
)

# Circular dendrogram using the option type = "circular".
fviz_dend(
  res.hc,
  cex = 0.5,
  k = 3,
  k_colors = "jco",
  type = "circular"
)

# Phylogenic trees
fviz_dend(
  res.hc,
  k = 3,
  k_colors = "jco",
  type = "phylogenic",
  repel = TRUE,
  phylo_layout = "layout.gem"
)

# Saving dendrogram into a large PDF page
pdf("dendrogram.pdf", width = 30, height = 15) # Open a PDF
p <-
  fviz_dend(res.hc,
            k = 3,
            cex = 1,
            k_colors = "jco") # Do plotting
print(p)
dev.off()


## Quiz: Repeat hierarchical clustering using "single" as linkage method.
