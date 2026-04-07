# Design of Data Analysis Pipeline for Precision Nutrition Analysis of Early Childhood Growth Trajectories

# Contributors:
Yue-Tong Chen,
Brinley Klievik,
Triyani Komang,
Eva Kranenburg

# The Sections Below Will Follow the CRISP-DM Framework:
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

# R Scripts in this Repository:
- '1.preprocessing_data.R' | R script containing the index heatmap depicted in Graph 7 of the CRISP-DM report
- '2.assessing_clustering_tendency.R' | Supplementary R script assessing clustering tendency
- '3.dist_computation.R' | R script containing the code to generate randomized subsets of data for Euclidean and Manhattan plots using set.seed()
- '4.clustering.R' | Supplementary R script assessing clustering
- '5.clust_validation.R' | Supplementary R script validating clustering
- 'YTC_Group Project R codes.R' | Main R script containing the codes for data cleaning, clustering, and profiling
- 'Assessing_clustering_tendency_zBMI_24m_biological_interpretation.R' | R script for generating Graphs 21 and 22 for biological interpretation of zBMI clusters

# Installation:
- Clone the repository
- Open R scripts in RStudio

# How to Run the Analysis:
Run the scripts in the following order:
- General Pipeline:
   - YTC_Group Project R codes.R (full pipeline: preprocessing → clustering → profiling)
- Specific Pipeline:
1. 1.preprocessing_data.R (cleanining data and generating the full_dataset_april.csv file)
2. 2.assessing_clustering_tendency.R (cluster tendency)
3. 3.dist_computation.R (code to visualize and assess Euclidean and Manhattan distances of zBMI at 24 months)
4. 4.clustering.R (code to cluster zBMI at 24 months)
5. 5.clust_validation.R (code for validating zBMI at 24 months clusters)
- Additional Script to Assess zBMI data at 24 months:
   - Assessing_clustering_tendency_zBMI_24m_biological_interpretation.R (assess cluster tendency; requires the cleaned and processed dataset)

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
This pipeline can be leveraged to identify predictors of characteristics of the child, including zBMI at 24 months, to aid in determining maternal dietary factors that influence growth outcomes and establish dietary strategies during pregnancy to promote healthy growth trajectories in the offspring.
This analysis may also aid in better understanding why child characteristics are heterogeneous and establish predictors, both individually and collectively.
Overall, by better understading how multi-dimension data influences offspring outcomes, this analysis pipeline may contribute to establishing more precise nutritional interventions for mothers rather than providing general one-size-fits-all guidelines.

# Model Limitations: 
- Cross-sectional, cannot infer causality
- Potential residual confounding
- Dietary intake may have reporting bias
- Small cluster size may result in reduced statistical power
- Findings may not be generalizable beyond this cohort

# Reproducibility:
- This analysis pipeline uses data and R scripts available on this repository, allowing for full reproducibility of this analysis pipeline.
- For Euclidean and Manhattan distances, set.seed() was applied to produce a smaller subset of random values for visualization.
- IDs of the individuals removed from final dataset (final_clean_data.csv) are listed in the CRISP-DM report.

# Ethical Considerations:
Ethical considerations for its implementation will also be made, particularly if any socioeconomic factors are found to be strongly associated with zBMI scores. Care must be taken to avoid stigmatization or inappropriate use of socioeconomic predictors in clinical decision-making.  
