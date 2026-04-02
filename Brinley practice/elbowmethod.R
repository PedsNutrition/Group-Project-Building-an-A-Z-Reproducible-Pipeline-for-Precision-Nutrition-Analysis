# ----------------------------------------
# Elbow Method for Household Income
# ----------------------------------------

# Load data
df <- read.csv("final_dataset.csv")

# Remove missing values for income
df <- df[!is.na(df$Household_income_index), ]

# Extract income variable
income <- df$Household_income_index

# Scale the data
income_scaled <- scale(income)

# Compute within-cluster sum of squares (WSS)
wss <- numeric(10)

set.seed(123)

for (k in 1:10) {
  kmeans_model <- kmeans(income_scaled, centers = k, nstart = 10)
  wss[k] <- kmeans_model$tot.withinss
}

# Plot elbow curve
plot(1:10, wss, type = "b",
     pch = 19,
     xlab = "Number of clusters (k)",
     ylab = "Within-cluster sum of squares",
     main = "Elbow Method for Household Income")