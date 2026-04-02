# Exploring Missingness & Data Cleaning for Growth Dataset.R
#
#
# Version:  1.0
#
# Date:     2026  Feb
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

# We will:
# 1. Explore the structure of the dataset
# 2. Quantify and visualize missingness
# 3. Identify implausible age and zBMI values
# 4. Clean growth data using WHO-style plausibility cut-offs
# 5. Generate a cleaned dataset for downstream analysis

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                  Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                44
#TOC>   2        Data loading                            60
#TOC>   3        Missingness exploration                 75
#TOC>   4        Distribution inspection                 120
#TOC>   5        Identifying implausible values          155
#TOC>   6        Data cleaning                           200
#TOC>   7        Post-cleaning diagnostics               240
#TOC>
#TOC> ==========================================================================

# ======    1  Packages  =======================================================

packages <- c("tidyverse", "naniar", "skimr", "ggplot2")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =    2  Data loading  =========================================================

# Set working directory if needed
# setwd("your/path/here")


#Import dataset to Global Environment: "mock_precision_growth_dataset.csv"
data <- mock_precision_growth_dataset #making a new dataframe from csv file

# Inspect structure
glimpse(data) #dataset contains 300 participants with 32 variables
summary(data) #still shows NAs in the dataset

# =====   3  Missingness exploration  ===========================================

# 3.1 Percent missing per variable

missing_summary <- data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary) #it shows that 5 variables have 10% missing data, including
                      # weight_12m_kg, ALT, Fiber_intake_g, Shannon diversity, 
                      # and WHO_zBMI_12m 

# 3.2 Visual inspection of missing data


vis_miss(data) #overall, there are 1.6% missing data among all variables 

# Missingness combinations
gg_miss_upset(data) #it visualized how many observations share the same 
                    # missingness pattern:


# 3.3 Missingness by subgroup (example: sex)


if ("sex" %in% names(data)) {
  
  data %>%
    group_by(sex) %>%
    summarise(across(everything(),
                     ~ mean(is.na(.)) * 100))
}

# ======    4  Distribution inspection  =========================================

# 4.1 zBMI distribution


if ("zbmi_24" %in% names(data)) {
  
  ggplot(data, aes(x = zbmi_24)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of zBMI at 24 months")
}


# 4.2 CRP distribution


if ("crp" %in% names(data)) {
  
  ggplot(data, aes(x = crp)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of CRP")
  
  # Log-transform CRP (common due to right skewness)
  data <- data %>%
    mutate(log_crp = log(crp))
  
  ggplot(data, aes(x = log_crp)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of log(CRP)")
}

# =====    5  Identifying implausible values  ===================================

# 5.1 Implausible age values

if ("age_months" %in% names(data)) {
  
  implausible_age <- data %>%
    filter(Age_24m_months < 0 | Age_24m_months > 60)
  
  print(implausible_age)
} #There is no "age_months" variable in the dataset, so we check what are the 
  #variable's names existed'

#5.1.1: check if the column actually exists
names(data) #the variable's name existed is "Age_24m_months" instead of "age_months"

#5.1.2: replace the variable's name
if ("Age_24m_months" %in% names(data)) {
  
  implausible_age <- data %>%
    filter(Age_24m_months < 0 | Age_24m_months > 60)
  
  print(implausible_age)
}
nrow(implausible_age) #This identifyies that 4 values that were implausible

# To show which row numbers in the original data
which(data$Age_24m_months < 0 | data$Age_24m_months > 60) 
# There are implausible values for participants 83 144 179 192, which are age -3, 
# -3, 120, and -3, respectively.


# 5.2 WHO-style plausibility cut-offs for zBMI

# WHO commonly flags z-scores < -5 or > +5 as implausible

if ("zbmi_24" %in% names(data)) {
  
  implausible_zbmi <- data %>%
    filter(zbmi_24 < -5 | zbmi_24 > 5)
  
  print(implausible_zbmi)
} ## this shows error because there is no zbmi_24 in the dataset.

#5.2.1: check if the column actually exists
names(data) #the variable's name existed is "zBMI_24m" instead of "zbmi_24"

#5.2.2: replace the variable's name
if ("zBMI_24m" %in% names(data)) {
  
  implausible_zbmi <- data %>%
    filter(zBMI_24m < -5 | zBMI_24m > 5)
  
  print(implausible_zbmi)
}

nrow(implausible_zbmi) #This identifyies that 1 value that was implausible

# To show which row numbers in the original data
which(data$zBMI_24m < -5 | data$zBMI_24m > 5) 
# There is an implausible zBMI value of "7.28471418" in the participant's ID 294
                                            

# 5.3 Extreme CRP values (possible acute infection)


if ("crp" %in% names(data)) {
  
  extreme_crp <- data %>%
    filter(crp > 10)
  
  print(extreme_crp)
} #It shows error because there is no variable's name as crp"

#5.3.1: check if the column "crp" actually exists
names(data) #the variable's name existed is "CRP" instead of "crp"

#5.2.2: replace the variable's name
if ("CRP" %in% names(data)) {
  
  extreme_crp <- data %>%
    filter(CRP > 10)
  
  print(extreme_crp)
}
nrow(extreme_crp) #This identifyies none of the CRP value was implausible

# Re-checking again the extreme CRP
which(data$CRP > 10) #There is no implausible value of CRP

# =====    6  Data cleaning  ===================================================

clean_data <- data

# Remove implausible age
if ("Age_24m_months" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(Age_24m_months >= 0 & Age_24m_months <= 60)
}

# Remove implausible zBMI
if ("zBMI_24m" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(zBMI_24m >= -5 & zBMI_24m <= 5)
}

# Optional: Remove extreme CRP > 10 mg/L
if ("CRP" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(CRP <= 10 | is.na(CRP))
}

# =====    7  Post-cleaning diagnostics  =======================================

# Compare sample size
cat("Original N:", nrow(data), "\n") #Number of participants in original dataset = 300
cat("Cleaned N:", nrow(clean_data), "\n") #Number of participants after data cleaning = 295

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
write_csv(clean_data, "clean_precision_growth_dataset.csv")




#===========================================================================#
####                    RESTRUCTURING DATASET                            ####
#===========================================================================#
View(clean_precision_growth_dataset) #first, view the clean dataset

prep_final_dataset <-clean_precision_growth_dataset #create new dataframe for restructuring

#show all column names
colnames(prep_final_dataset)

# Exclude variables with 9.49 - 10.2% missing data
prep_final_dataset <- subset(prep_final_dataset, select = -c(WHO_zBMI_12m,
                                                   Weight_12m_kg, Shannon_diversity, 
                                                   Fiber_intake_g, ALT))

# Reorder by column name
prep_final_dataset <- prep_final_dataset[, c("ID", "Sex", "Age_24m_months", "Gestational_age_weeks",
                                   "Birth_weight_g", "Weight_6m_kg","Weight_24m_kg", 
                                   "Birth_length_cm", "Length_6m_cm", "Length_12m_cm", 
                                   "Length_24m_cm", "WHO_zBMI_birth","zBMI_24m", 
                                   "Stunted_24m", "Head_circumference_cm", "Maternal_BMI",
                                   "Energy_intake_kcal", "Protein_intake_g", 
                                   "Dietary_pattern_score", "Ultra_processed_score", "Fasting_glucose", 
                                   "CRP", "Maternal_education_years", "Household_income_index",
                                   "Firmicutes_Bacteroidetes_ratio", "SCFA_index" )]

View(prep_final_dataset)

#Save New Dataset as "final_dataset_april"
library(readr)
write_csv(prep_final_dataset, "final_dataset_april.csv")

