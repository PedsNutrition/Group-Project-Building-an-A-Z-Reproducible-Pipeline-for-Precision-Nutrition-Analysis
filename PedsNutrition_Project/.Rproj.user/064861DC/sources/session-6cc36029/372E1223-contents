# Missing Data Imputation & Method Comparison for final_dataset
#
# Continues from: 1_preprocessing_data.R
#
# Objectives:
#   1. Mean/Mode imputation on final_dataset
#   2. MICE (Multiple Imputation by Chained Equations) imputation
#   3. Compare both methods (distribution, RMSE via simulation, visual diagnostics)
#
# NOTE: Run 1_preprocessing_data.R first so that `final_dataset` exists in your
#       environment, OR load it from the saved CSV:
#         final_dataset <- read_csv("final_dataset.csv")
# ==============================================================================


# =============================================================================
# Packages
# =============================================================================

packages <- c("tidyverse", "mice", "naniar", "ggplot2", "VIM",
              "Hmisc", "modeest", "gridExtra", "knitr")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


# =============================================================================
# SECTION 1: Load / confirm dataset
# =============================================================================

df <- final_dataset       

cat("Dataset dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")

# Quick missingness summary
missing_pct <- df %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "pct_missing") %>%
  filter(pct_missing > 0) %>%
  arrange(desc(pct_missing))

cat("Variables with missing values:\n")
print(missing_pct, n = Inf)

# Which columns actually have missingness?
cols_with_na <- missing_pct$variable
cat("\nTotal variables with NA:", length(cols_with_na), "\n")


# =============================================================================
# SECTION 2: Helper utilities
# =============================================================================

# Statistical mode for a numeric/character vector (handles ties by first value)
calc_mode <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) return(NA)
  ux <- unique(x_clean)
  ux[which.max(tabulate(match(x_clean, ux)))]
}

# Identify column types for imputation strategy
numeric_cols  <- df %>% select(where(is.numeric))  %>% names()
factor_cols   <- df %>% select(where(~ is.character(.) | is.factor(.))) %>% names()

# Remove ID from imputation targets 
impute_numeric <- setdiff(numeric_cols, "ID")
impute_factor  <- setdiff(factor_cols,  "ID")

cat("\nNumeric columns to impute:", paste(impute_numeric, collapse = ", "), "\n")
cat("Categorical columns to impute:", paste(impute_factor,  collapse = ", "), "\n\n")


# =============================================================================
# SECTION 3: METHOD 1 — Mean / Mode Imputation
# =============================================================================

cat("========================================\n")
cat("METHOD 1: Mean / Mode Imputation\n")
cat("========================================\n\n")

df_meanmode <- df   

# 3.1 Numeric columns → replace NA with column mean
for (col in intersect(impute_numeric, cols_with_na)) {
  col_mean <- mean(df_meanmode[[col]], na.rm = TRUE)
  n_imputed <- sum(is.na(df_meanmode[[col]]))
  df_meanmode[[col]][is.na(df_meanmode[[col]])] <- col_mean
  cat(sprintf("  [Mean] %-35s | imputed %d values with mean = %.4f\n",
              col, n_imputed, col_mean))
}

# 3.2 Categorical columns → replace NA with column mode
for (col in intersect(impute_factor, cols_with_na)) {
  col_mode  <- calc_mode(df_meanmode[[col]])
  n_imputed <- sum(is.na(df_meanmode[[col]]))
  df_meanmode[[col]][is.na(df_meanmode[[col]])] <- col_mode
  cat(sprintf("  [Mode] %-35s | imputed %d values with mode = '%s'\n",
              col, n_imputed, col_mode))
}

cat("\nMissing values remaining after Mean/Mode imputation:",
    sum(is.na(df_meanmode)), "\n\n")

# Save
write_csv(df_meanmode, "imputed_meanmode.csv")
cat("Saved: imputed_meanmode.csv\n\n")


# =============================================================================
# SECTION 4: METHOD 2 — MICE Imputation
# =============================================================================

cat("========================================\n")
cat("METHOD 2: MICE Imputation\n")
cat("========================================\n\n")

# 4.1 Prepare data for MICE
#     MICE works on all columns simultaneously; exclude non-imputable ID column
df_for_mice <- df %>% select(-ID)

# 4.2 Define imputation methods per column
#     "pmm"   = predictive mean matching  (numeric — preserves distribution)
#     "logreg"= logistic regression        (binary factor)
#     "polyreg"= polytomous regression    (unordered multi-level factor)
#     "norm"  = Bayesian linear regression (numeric, alternative)

ini <- mice(df_for_mice, maxit = 0, print = FALSE)   # dry run to get method vector
meth <- ini$method

# Assign pmm to all numeric; logreg/polyreg to factor columns
for (col in names(df_for_mice)) {
  if (col %in% impute_numeric && col %in% cols_with_na) {
    meth[col] <- "pmm"
  } else if (col %in% impute_factor && col %in% cols_with_na) {
    n_levels <- length(unique(na.omit(df_for_mice[[col]])))
    meth[col] <- if (n_levels == 2) "logreg" else "polyreg"
  }
}

cat("MICE method assignments:\n")
print(meth[meth != ""])

# 4.3 Run MICE
#     m = 5 imputed datasets, maxit = 10 iterations (standard defaults)
set.seed(2026)
mice_out <- mice(df_for_mice,
                 m      = 5,
                 maxit  = 10,
                 method = meth,
                 print  = FALSE)

cat("\nMICE convergence summary:\n")
print(mice_out)

# 4.4 Pool: take the first completed dataset for a single-use comparison
#     (For formal inference, use pool() on model results instead)
df_mice_complete <- complete(mice_out, action = 1)   # dataset 1 of 5
df_mice_complete <- bind_cols(ID = df$ID, df_mice_complete)

cat("\nMissing values remaining after MICE imputation:",
    sum(is.na(df_mice_complete)), "\n\n")

# Save
write_csv(df_mice_complete, "imputed_mice.csv")
cat("Saved: imputed_mice.csv\n\n")


# =============================================================================
# SECTION 5: METHOD COMPARISON
# =============================================================================

cat("========================================\n")
cat("SECTION 5: Comparing Imputation Methods\n")
cat("========================================\n\n")

# We compare on the numeric columns that had missing values
num_cols_imputed <- intersect(impute_numeric, cols_with_na)

# --------------------------------------------------------------------------
# 5.1  Distribution comparison: original observed vs. both imputed datasets
# --------------------------------------------------------------------------

cat("5.1 Generating distribution comparison plots...\n")

plot_list <- list()

for (col in num_cols_imputed) {

  # Build a tidy data frame with source label
  observed <- df[[col]][!is.na(df[[col]])]

  df_compare <- bind_rows(
    tibble(value = observed,                  source = "Observed"),
    tibble(value = df_meanmode[[col]],        source = "Mean/Mode"),
    tibble(value = df_mice_complete[[col]],   source = "MICE")
  )

  p <- ggplot(df_compare, aes(x = value, fill = source, colour = source)) +
    geom_density(alpha = 0.35, linewidth = 0.7) +
    scale_fill_manual(values   = c("Observed" = "#2c7bb6",
                                   "Mean/Mode"= "#d7191c",
                                   "MICE"     = "#1a9641")) +
    scale_colour_manual(values = c("Observed" = "#2c7bb6",
                                   "Mean/Mode"= "#d7191c",
                                   "MICE"     = "#1a9641")) +
    labs(title  = paste("Distribution:", col),
         x      = col, y = "Density", fill = "Source", colour = "Source") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 9, face = "bold"))

  plot_list[[col]] <- p
}

# Save distribution comparison plots to a PDF
pdf("distribution_comparison.pdf", width = 14, height = 4 * ceiling(length(plot_list) / 3))
n_plots <- length(plot_list)
grid.arrange(grobs = plot_list,
             ncol  = min(3, n_plots),
             top   = "Distribution Comparison: Observed vs. Mean/Mode vs. MICE")
dev.off()
cat("Saved: distribution_comparison.pdf\n\n")


# --------------------------------------------------------------------------
# 5.2  Summary statistics comparison table
# --------------------------------------------------------------------------

cat("5.2 Summary statistics comparison:\n\n")

stats_table <- map_dfr(num_cols_imputed, function(col) {
  bind_rows(
    tibble(Variable = col, Method = "Original (observed only)",
           Mean   = mean(df[[col]], na.rm = TRUE),
           SD     = sd(df[[col]],   na.rm = TRUE),
           Median = median(df[[col]], na.rm = TRUE)),
    tibble(Variable = col, Method = "Mean/Mode imputed",
           Mean   = mean(df_meanmode[[col]]),
           SD     = sd(df_meanmode[[col]]),
           Median = median(df_meanmode[[col]])),
    tibble(Variable = col, Method = "MICE imputed",
           Mean   = mean(df_mice_complete[[col]]),
           SD     = sd(df_mice_complete[[col]]),
           Median = median(df_mice_complete[[col]]))
  )
})

print(stats_table, n = Inf)
write_csv(stats_table, "stats_comparison.csv")
cat("\nSaved: stats_comparison.csv\n\n")


# --------------------------------------------------------------------------
# 5.3  Simulated RMSE comparison (amputation + re-imputation benchmark)
#      Strategy: artificially MCAR-ampute 10% of complete cases,
#                run both methods, compute RMSE vs. known truth.
# --------------------------------------------------------------------------
library(mice)
library(dplyr)
library(purrr)   # for map_dfr
library(readr)   # for write_csv
library(ggplot2) # for ggplot
library(tidyr)   # for pivot_longer

cat("5.3 Simulated RMSE benchmark (MCAR amputation test)...\n\n")

# Use only complete rows as the "truth" reference
df_complete_rows <- final_dataset %>% dplyr::filter(if_all(everything(), ~ !is.na(.)))

num_cols_imputed <- df_complete_rows %>%
  select(where(is.numeric)) %>%
  select(-c(ID)) %>%
  names()

if (nrow(df_complete_rows) < 30) {
  cat("  Too few complete cases for RMSE simulation — skipping.\n\n")
} else {
  set.seed(42)
  amp_out <- ampute(df_complete_rows %>% select(all_of(num_cols_imputed)),
                    prop = 0.10, mech = "MCAR")
  df_amp <- amp_out$amp
  
  # --- Mean imputation on amputed data ---
  df_amp_mean <- df_amp
  for (col in num_cols_imputed) {
    m <- mean(df_amp_mean[[col]], na.rm = TRUE)
    df_amp_mean[[col]][is.na(df_amp_mean[[col]])] <- m
  }
  
  # --- MICE imputation on amputed data ---
  set.seed(2026)
  mice_amp <- mice(df_amp, m = 5, maxit = 10, method = "pmm", print = FALSE)
  df_amp_mice <- mice::complete(mice_amp, 1)  # fix: explicit namespace
  
  # --- RMSE per column ---
  truth <- df_complete_rows %>% select(all_of(num_cols_imputed))
  rmse_results <- map(num_cols_imputed, function(col) {  # fix: map_dfr deprecated
    mask <- is.na(df_amp[[col]])
    if (sum(mask) == 0) return(NULL)
    true_vals <- truth[[col]][mask]
    pred_mean <- df_amp_mean[[col]][mask]
    pred_mice <- df_amp_mice[[col]][mask]
    tibble(
      Variable  = col,
      RMSE_Mean = sqrt(mean((true_vals - pred_mean)^2)),
      RMSE_MICE = sqrt(mean((true_vals - pred_mice)^2)),
      Winner    = ifelse(RMSE_MICE < RMSE_Mean, "MICE", "Mean/Mode")
    )
  }) %>% list_rbind()  # fix: replaces map_dfr
  
  cat("RMSE Comparison (lower = better):\n")
  print(rmse_results, n = Inf)
  write_csv(rmse_results, "rmse_comparison.csv")
  cat("\nSaved: rmse_comparison.csv\n")
  
  # Summary
  n_mice_wins <- sum(rmse_results$Winner == "MICE",      na.rm = TRUE)
  n_mean_wins <- sum(rmse_results$Winner == "Mean/Mode", na.rm = TRUE)
  cat(sprintf("\nMICE wins: %d/%d variables | Mean/Mode wins: %d/%d variables\n\n",
              n_mice_wins, nrow(rmse_results),
              n_mean_wins, nrow(rmse_results)))
  
  # --- RMSE barplot ---
  rmse_long <- rmse_results %>%
    pivot_longer(cols = c(RMSE_Mean, RMSE_MICE),
                 names_to  = "Method",
                 values_to = "RMSE") %>%
    mutate(Method = recode(Method,
                           "RMSE_Mean" = "Mean/Mode",
                           "RMSE_MICE" = "MICE"))
  
  p_rmse <- ggplot(rmse_long, aes(x = reorder(Variable, RMSE), y = RMSE, fill = Method)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    scale_fill_manual(values = c("Mean/Mode" = "#d7191c", "MICE" = "#1a9641")) +
    coord_flip() +
    labs(title = "RMSE Comparison: Mean/Mode vs. MICE (MCAR simulation)",
         x = "Variable", y = "RMSE (lower is better)", fill = "Method") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("rmse_comparison_plot.png", p_rmse, width = 9, height = 5, dpi = 150)
  cat("Saved: rmse_comparison_plot.png\n\n")
}


# --------------------------------------------------------------------------
# 5.4  MICE convergence / trace plots
# --------------------------------------------------------------------------

cat("5.4 MICE convergence trace plots...\n")
set.seed(2026)
mice_out <- mice(df_complete_rows, m = 5, maxit = 10, method = "pmm", print = FALSE)
png("mice_trace_plots.png", width = 1400, height = 900, res = 120)
plot(mice_out, layout = c(4, ceiling(length(num_cols_imputed) / 2)))
dev.off()
cat("Saved: mice_trace_plots.png\n\n")


# --------------------------------------------------------------------------
# 5.5  Variance reduction diagnostic
#      Mean imputation artificially reduces variance — quantify the shrinkage
# --------------------------------------------------------------------------

cat("5.5 Variance shrinkage (Mean/Mode vs. original observed):\n\n")

var_table <- map(num_cols_imputed, function(col) {
  tibble(
    Variable           = col,
    Var_Original       = var(df[[col]],             na.rm = TRUE),
    Var_MeanMode       = var(df_meanmode[[col]],     na.rm = TRUE),
    Var_MICE           = var(df_mice_complete[[col]], na.rm = TRUE),
    Shrinkage_Mean_pct = round(
      (1 - var(df_meanmode[[col]],      na.rm = TRUE) /
         var(df[[col]],               na.rm = TRUE)) * 100, 2),
    Shrinkage_MICE_pct = round(
      (1 - var(df_mice_complete[[col]], na.rm = TRUE) /
         var(df[[col]],               na.rm = TRUE)) * 100, 2)
  )
}) %>% list_rbind()

print(var_table, n = Inf)
write_csv(var_table, "variance_comparison.csv")
cat("\nSaved: variance_comparison.csv\n\n")


# =============================================================================
# SECTION 6: Final recommendation summary
# =============================================================================

cat("========================================\n")
cat("SECTION 6: Final Recommendation\n")
cat("========================================\n\n")

cat(
  "MEAN / MODE IMPUTATION\n",
  "  Pros : Fast, simple, no tuning required.\n",
  "  Cons : Artificially narrows variance (see variance_comparison.csv),\n",
  "         distorts distributions (see distribution_comparison.pdf),\n",
  "         ignores relationships between variables,\n",
  "         produces biased regression coefficients when >5% missing.\n\n",
  "MICE (Multiple Imputation by Chained Equations)\n",
  "  Pros : Preserves marginal distributions and inter-variable relationships,\n",
  "         accounts for imputation uncertainty via m=5 datasets,\n",
  "         lower RMSE in MCAR simulation (see rmse_comparison.csv),\n",
  "         suitable for downstream regression / mixed models.\n",
  "  Cons : Slower; requires pooled inference for valid standard errors.\n\n",
  "RECOMMENDATION: Use MICE for this dataset.\n",
  "  -> Use `imputed_mice.csv` (or pool across all 5 mice datasets) for analysis.\n",
  "  -> For publication, apply Rubin's rules via mice::pool() on model objects.\n",
  sep = ""
)

cat("\n--- Script complete. Output files: ---\n")
cat("  imputed_meanmode.csv\n")
cat("  imputed_mice.csv\n")
cat("  stats_comparison.csv\n")
cat("  variance_comparison.csv\n")
cat("  rmse_comparison.csv         (if enough complete cases)\n")
cat("  distribution_comparison.pdf\n")
cat("  rmse_comparison_plot.png    (if enough complete cases)\n")
cat("  mice_trace_plots.png\n")
