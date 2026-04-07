# Design of Data Analysis Pipeline for Precision Nutrition Analysis of Early Childhood Growth Trajectories

# Contributors:
Yue-Tong Chen,
Brinley Klievik,
Triyani Komang,
Eva Kranenburg

# The sections below will follow the CRISP-DM framework:
- Project Understanding: Research question and exposures
- Data Understanding: Exploring the dataset and data distributions
- Data Preparation: Cleaning data, handling missingness, and transforming variables as appropriate
- Modelling: Unsupervised clustering of zBMI scores at 24 months
- Evaluation: Assessment of clustering validity using statistical and biological approaches
- Deployment: Generated precision nutrition insights and potential application

# Project Overview:
This repository applies the CRISP-DM framework to investigate whether maternal dietary and socioeconomic factors are associated with early childhood growth patterns at 24 months.
Using BMI-for-age z-scores (zBMI), standardized by the WHO Child Growth Standards, this pipeline is designed to identify distinct growth subgroups through unsupervised clustering. These subgroups will then be used to evaluate associations with maternal exposures and generate precision nutrition insights.

# Research Question:
Are maternal dietary, gut microbiome, and socioeconomic factors associated with early childhood growth patterns, as defined by BMI-for-age z scores (zBMI)-based cluster patterns, at 24 months of age?

# Dataset Description:
This dataset contains data collected from ~300 children at 24 months of age, and includes 32 variables across multiple domains:
- Maternal dietary factors: energy intake (kcal), fiber intake (g), protein intake (g), ultra-processed food score, dietary pattern score
- Microbiome factors: Firmicutes:Bacteroidetes ratio, Shannon diversity index, short-chain fatty acid (SCFA) index
- Socioeconomic factors: household income, maternal education (years)
- Maternal health factors: maternal BMI (kg/m²), gestational age (weeks)
- Clinical biomarkers: fasting glucose (mmol/L), C-reactive protein (CRP), alanine aminotransferase (ALT)
- Child growth measures: sex, age, head circumference, weight and length at multiple time points (0, 6, 12, 24 months), BMI, and BMI-for-age z-scores (zBMI)
zBMI at 24 months was used for clustering analysis.
Ultra-processed food score, energy intake, and household income were used as maternal dietary and socioeconomic exposures for downstream associations.

# Files in this repository:
- 'YTC_Group Project R codes.R' | Main R script containing the codes for data cleaning, clustering, and profiling
- 'Assessing_clustering_tendency_zBMI_24m_biological_interpretation.R' | R script for generating Graphs 21 and 22 for biological interpretation of zBMI clusters
- 'final_dataset_april.csv' | Cleaned dataset used in the analysis
- 'mock_precision_growth_datset.csv' | Original full dataset used in the preprocessing steps
- 'zBMI_24m Clustering Graphs.pptx' | PowerPoint of zBMI Clustering graphs
- 'NFS1218_Pediatric Nutrition Pipeline.docx' | CRISP-DM report
- 'README.docx' | Draft of the README file

# Installation:
- Clone the repository
- Open R scripts in RStudio

# How to Run the Analysis:
Run the scripts in the following order:
1. YTC_Group Project R codes.R (full pipeline: preprocessing → clustering → profiling)
2. Assessing_clustering_tendency_zBMI_24m_biological_interpretation.R (assess cluster tendency; requires the cleaned and processed dataset)

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
    - Normality assessed using histograms
    - Skewed variables (e.g., CRP) were log1p-transformed
    - Continuous variables were scaled prior to clustering

# Analytical Approach:
1. Data preparation
    - Cleaned and standardized dataset was used for analysis
3. Clustering tendency assessment
    - Euclidean and Manhattan distance metrics
    - Hopkins statistic used to confirm clustering
4. Clustering analysis
    - Optimal cluster number determined using:
         - Elbow method
         - Silhouette analysis
         - NbClust (majority voting across indices)
    - Unsupervised clustering methods:
         - K-means
         - PAM (Partitioning Around Medoids)
         - Hierarchical clustering
5. Cluster validation
    - Clustering validity was assessed using complementary approaches to ensure robust and biologically meaningful cluster identification:
         - Internal validation metrics: Connectivity, Dunn Index, and Silhouette Width were used to evaluate cluster cohesion and separation
         - Method comparison: clustering results were compared across K-means, PAM, and hierarchical methods to assess consistency
         - Cluster number validation: agreement across NbClust, Elbow method, and Silhouette analysis supported the selection of k=3
         - Biological validation: identified clusters reflected biologically plausible growth patterns (lower, normal, higher zBMI scores at 24 months), supporting interpretability
6. Cluster profiling
    - zBMI clusters were merged with our exposures of interest (ultra-processed food score, total energy intake, and household income) to evaluate potential associations with early childhood growth patterns.
  
# Precision Nutrition Insights:
This data pipeline identified growth trajectory clusters providing a framework to evaluate how maternal nutrition and socioeconomic factors relate to child growth, enabling targeted, evidence-based nutritional guidance.

# Model Limitations: 
- Cross-sectional, cannot infer causality
- Potential residual confounding
- Dietary intake may have reporting bias
- Small cluster size may result in reduced statistical power
- Findings may not be generalizable beyond this cohort

# Reproducibility:
- This analysis pipeline uses data and R scripts available on this repository, allowing for full reproducibility of this analysis pipeline.
- For Euclidean and Manhattan distances, set.seed() was applied to produce a smaller subset of random values for visualization.
- IDs of the individuals removed from final dataset (final_dataset_april.csv) are listed in the CRISP-DM report.

# Ethical Considerations:
Ethical considerations for its implementation will also be made, particularly if any socioeconomic factors are found to be strongly associated with zBMI scores. Care must be taken to avoid stigmatization or inappropriate use of socioeconomic predictors in clinical decision-making.  
