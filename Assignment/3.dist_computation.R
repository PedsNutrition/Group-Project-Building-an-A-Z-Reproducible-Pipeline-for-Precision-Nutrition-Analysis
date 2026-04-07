# DISTANCE MATRIX COMPUTATION.R
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

# We will start by computing the distance matrix using different methods. 
# Next, we will compute the distance matrix after standarizing the data.  
# Finally, we will visualize the distance matrices.

#RESOURCES:http://www.sthda.com/english/

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                          39
#TOC>   2        Data preparation                                  52      
#TOC>   3        Computing distances                               62
#TOC>   4        Data standarization                               78                
#TOC>
#TOC> ===========================================================================

# =    1  Packages  =============================================================

if (! require(factoextra, quietly=TRUE)) {
  install.packages(factoextra)
  library(factoextra)
}

if (! require(hopkins, quietly=TRUE)) {
  install.packages(hopkins)
  library(hopkins)
}


# =    2  Data preparation  ====================================================

# We will use a R-built in dataset
# - final_clean_data

# Our group will use "final_clean_data", but first check for missing values
View(final_clean_data)

# Subset of the data
set.seed(123)
ss <- sample(1:50, 15) # Take 15 random rows
df <- final_clean_data[ss, ] # Subset the 15 rows


# =   3  Computing distances  ====================================================

# 3.1.Using euclidean metric
dist.eucl <- dist(df, method = "euclidean") #NAs introduced by coercion.

# Convert characters into numeric
df_numeric <- df[, sapply(df, is.numeric)]

# Replace the original "df" as "df_numeric"
dist.eucl <- dist(df_numeric, method = "euclidean")

# 3.2 Using manhattan metric
dist.manh <- dist(df, method = "manhattan") #NAs introduced by coercion.

# Replace the original "df" as "df_numeric"
dist.manh <- dist(df_numeric, method = "manhattan")

# 3.3 Let's visualize the distance matrices.
raw_eucl_m<-fviz_dist(dist.eucl)
print(raw_eucl_m)

raw_manh_m<-fviz_dist(dist.manh)
print(raw_manh_m)

# Quiz: Do you observe any differences? Why?
# They are nearly identical, because variables are on similar scales, 
# and the data doesn't have extreme outliers in individual dimensions.


# =   4  Data standarization  ====================================================

# Let's repeat the experiment using standarized data this time. 
df.scaled <- scale(df_numeric) # Standardize the variables.
# Using df contains all numeric

# Using euclidean metric
dist.eucl <- dist(df.scaled, method = "euclidean")

# Using manhattan metric
dist.manh <- dist(df.scaled, method = "manhattan")

# Let's visualize the distance matrices.
std_eucl_m<-fviz_dist(dist.eucl)
print(std_eucl_m)

std_manh_m<-fviz_dist(dist.manh)
print(std_manh_m)

# Quiz: Do you observe any differences? Why?
# Yes, they show meaningful differences between the two metrics.
# The value of Euclideian ranges from 0 - 9, whilst Manhattan ranges from 0 - 50.
# Cluster boundaries in Euclidian is slightly blurred between groups;
# In Manhattan shows more clearly defined block separation.

# After standardization, the Manhattan metric amplifies separation more than Euclidean,
# because it sums absolute differences across all dimensions without the 
# effect of square-rooting. 
# Manhattan is better for highlighting outliers and separating clusters;
# more sensitive to cumulative differences across many variables; and 
# more sensitive to noise in high dimensions


