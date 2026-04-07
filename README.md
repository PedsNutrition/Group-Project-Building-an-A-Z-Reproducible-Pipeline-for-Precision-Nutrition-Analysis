# Design of Data Analysis Pipeline for Precision Nutrition Analysis of Early Childhood Growth Trajectories

# Contributors:
Yue-Tong Chen,
Brinley Klievik,
Triyani Komang,
Eva Kranenburg

# Project Overview:
This repository applies the CRISP-DM framework to investigate whether maternal dietary and socioeconomic factors are associated with early childhood growth patterns at 24 months.
Using BMI-for-age z-scores (zBMI), standardized by the WHO Child Growth Standards, this pipeline is designed to identify distinct growth subgroups through unsupervised clustering. These subgroups will then be used to evaluate associations with maternal exposures and generate precision nutrition insights.

# Research Question:
Are maternal dietary, gut microbiome, and socioeconomic factors associated with early childhood growth patterns, as defined by BMI-for-age z scores (zBMI)-based cluster patterns, at 24 months of age?

# Dataset Description
This dataset contains data collected from ~300 children at 24 months of age, and includes 32 variables across multiple domains:
- Maternal dietary factors: energy intake (kcal), fiber intake (g), protein intake (g), ultra-processed food score, dietary pattern score
- Microbiome factors: Firmicutes:Bacteroidetes ratio, Shannon diversity index, short-chain fatty acid (SCFA) index
- Socioeconomic factors: household income, maternal education (years)
- Maternal health factors: maternal BMI (kg/m²), gestational age (weeks)
- Clinical biomarkers: fasting glucose (mmol/L), C-reactive protein (CRP), alanine aminotransferase (ALT)
- Child growth measures: sex, age, weight and length at multiple time points (0, 6, 12, 24 months), BMI, and BMI-for-age z-scores (zBMI)
zBMI at 24 months was used for clustering analysis.

# Installation
- Clone the repository
- Download and open the R scripts

# How to Run the Analysis
Run the scripts in the following order:
1. Preprocessing data
2. 

# Data Cleaning & Preparation:
The following preprocessing steps were applied:
1. Growth data cleaning
   - Applied WHO cut-offs to remove biologically implausible zBMI values (-5 ≤ zBMI ≤ 5)
   - Removed implausible age values
2. Outlier handling
   - Outliers were assessed visually using histograms
   - Any biologically implausible data was removed
3. Missing data
    - Missing data was assessed (heatmaps, UpSet plots)
    - No missing data in key variables -> no imputations applied
4. Data transformation
    - Normality assesed using histograms
    - Skewed variables (e.g., CRP) were log1p-transformed
    - Continuous variables were scaled prior to clustering

# Analytical Approach
1. Data was cleaned and missingness was address
2. Clustering tendency assessment
    - Euclidian and Manhattan distance metrics
    - Hopkins statistics used to confirm clustering
3. Clustering analysis
    - Optimal cluster number determined using:
         - Elbow method
         - Silhouette analysis
         - NbClust
    - Clustering methods:
         - K-means
         - PAM (Partitioning Around Medoids)
         - Hierarchical clustering

