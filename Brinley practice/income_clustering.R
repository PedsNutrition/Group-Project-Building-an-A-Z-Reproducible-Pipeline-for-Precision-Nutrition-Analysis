# ----------------------------------------
# Household Income Clustering Analysis
# ----------------------------------------

# Load data
df <- read.csv("final_dataset.csv")

# Remove missing values
df <- df[!is.na(df$Household_income_index), ]

# Extract income variable
income <- df$Household_income_index

# Scale data
income_scaled <- scale(income)

# K-means clustering (k = 3)
set.seed(123)
kmeans_income <- kmeans(income_scaled, centers = 3)

# Add cluster labels to dataset
df$income_cluster <- kmeans_income$cluster

# Load library
library(ggplot2)

# Create histogram plot
plot <- ggplot(df, aes(x = Household_income_index, fill = as.factor(income_cluster))) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  labs(
    title = "Household Income Clusters (k = 3)",
    x = "Household Income Index",
    y = "Count",
    fill = "Cluster"
  ) +
  theme_minimal()

# Save plot
ggsave("income_plot.png", plot = plot)

# Cluster summaries
aggregate(df$Household_income_index,
          by = list(cluster = df$income_cluster),
          mean)

aggregate(df$zBMI_24m,
          by = list(cluster = df$income_cluster),
          mean)

# Cluster sizes (optional)
table(df$income_cluster)
