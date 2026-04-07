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
This dataset contains data collected from ~300 children at 24 months of age, and includes 32 variables across multiple domains (maternal dietary factors, microbiome factors, socioeconomic factors, maternal health factors, clinical biomakers, and child growth factors).

# Installation
- Clone the repository
- Download and open the R scripts

# Data Cleaning & Preparation:
The following steps were applied to ensure quality data:
1. Growth data cleaning
   - Applied WHO cut-offs to remove biologically implausible zBMI values (-5 ≤ zBMI ≤ 5)
   - Removed implausible age values
2. Outlier handling
   - Outliers were assessed visually (by histograms)
   - Any biologically implausible data was removed before analysis
3. Missing data
    - Missing data was assessed
    - No missing data in key variables --> no imputations applied
6. Data transformation
