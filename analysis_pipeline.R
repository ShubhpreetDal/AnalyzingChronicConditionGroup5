# -------------------------------
# Analysis Pipeline (Phase 1)
# Data Cleaning & Decoding
# -------------------------------

# Load custom functions and mappings
source("R/clean_transform.R")
source("R/value_mappings.R")

# Step 1: Load raw data
file_path <- "data-raw/pumf_cchs.csv"
df_raw <- load_cchs_data(file_path)

# Step 2: Decode categorical values
df_decoded <- decode_cchs_values(df_raw, value_mappings)

# Step 3: Analyze 12–17 age group target variables
analyze_underage_targets(df_decoded)

# Step 4: Drop respondents aged 12–17 based on the analysis (to improve modeling quality)
filtered_decoded_data <- drop_underage_group(df_decoded)

# Step 5: Save dataset as internal package data
usethis::use_data(filtered_decoded_data, overwrite = TRUE)

# Step 6: Save as CSV for manual inspection
dir.create("data-csv", showWarnings = FALSE)
write.csv(filtered_decoded_data, "data-csv/filtered_decoded_data.csv", row.names = FALSE)

# Completion log
message("Phase 1 complete: Data loaded, decoded, filtered, and saved.")



# -------------------------------
# Phase 2: Descriptive Stats & Categorical Transformation
# -------------------------------

source("R/describe_stats.R")
source("R/transform_categorical.R")
source("R/visualization.R")

# Step 1: Generate descriptive statistics and Visulaization before transformation
initial_stats <- describe_categorical(filtered_decoded_data)
dir.create("Statistics", showWarnings = FALSE)
write.csv(initial_stats, "Statistics/categorical_descriptive_statistics.csv", row.names = FALSE)

generate_barplots(filtered_decoded_data, output_dir = "Plots/Before_Transformation")


# Step 2: Apply transformation logic
transformed_data <- transform_categorical(filtered_decoded_data)

# Step 3: Save transformed dataset
write.csv(transformed_data, "data-csv/cleaned_transformed_data.csv", row.names = FALSE)
usethis::use_data(transformed_data, overwrite = TRUE)

# Step 4: Descriptive stats after transformation
final_stats <- describe_categorical(transformed_data)
write.csv(final_stats, "Statistics/final_categorical_summary.csv", row.names = FALSE)

generate_barplots(transformed_data, output_dir = "Plots/After_Transformation")

message("Phase 2 complete: Categorical transformation and descriptive stats done.")



# -------------------------------
# Phase 3: Final Cleanup of Sparse & Ambiguous Levels
# -------------------------------

source("R/final_transform.R")

# Load transformed_data saved in .rda
load("data/transformed_data.rda")

# Step 1: Apply final transformation rules (e.g., rare level merges, unknown → mode)
final_cleaned_data <- final_transform(transformed_data)

# Step 2: Save final dataset as CSV and internal RDA
write.csv(final_cleaned_data, "data-csv/final_cleaned_data.csv", row.names = FALSE)
usethis::use_data(final_cleaned_data, overwrite = TRUE)

# Step 3: Generate descriptive stats for final cleaned data
final_stats <- describe_categorical(final_cleaned_data)
write.csv(final_stats, "Statistics/final_variable_summary.csv", row.names = FALSE)

# Step 4: Generate final visualization
generate_barplots(final_cleaned_data, output_dir = "Plots/Final_Visualization")

message("Phase 3 complete: Final cleanup, saving, and visualization done.")


# -------------------------------
# Phase 4: Bivariate Analysis
# -------------------------------
source("R/bivariate_analysis.R")

targets <- c(
  "Has a high blood pressure",
  "Has diabetes",
  "Cardiovascular condition (Heart disease or stroke)"
)

for (target in targets) {
  run_bivariate_analysis(final_cleaned_data, target)
}

message("Phase 4 complete: Bivariate chi-square analysis and plots generated.")

# -------------------------------
# Phase 5: Baseline Modeling (LogReg, Decision Tree)
# -------------------------------


# Load modeling function
source("R/baseline_models.R")

# Load final dataset
data("final_cleaned_data")
final_cleaned_data <- janitor::clean_names(final_cleaned_data)

# Prepare model-specific datasets
model1_df <- final_cleaned_data %>%
  select(-has_diabetes,
         -cardiovascular_condition_heart_disease_or_stroke,
         -high_blood_pressure_took_medication_1_month)

model2_df <- final_cleaned_data %>%
  select(-cardiovascular_condition_heart_disease_or_stroke)

model3_df <- final_cleaned_data

# Run baseline models
run_baseline_models(model1_df, "has_a_high_blood_pressure")
run_baseline_models(model2_df, "has_diabetes")
run_baseline_models(model3_df, "cardiovascular_condition_heart_disease_or_stroke")


# -------------------------------
# Phase 6: Final Logistic Regression Model & Interpretation
# -------------------------------

source("R/train_final_logreg.R")
source("R/plot_logreg_coefficients.R")

# Load final dataset
data("final_cleaned_data")
final_cleaned_data <- janitor::clean_names(final_cleaned_data)

# Create model-specific datasets
model1_df <- final_cleaned_data %>% select(-has_diabetes, -cardiovascular_condition_heart_disease_or_stroke, -high_blood_pressure_took_medication_1_month)
model2_df <- final_cleaned_data %>% select(-cardiovascular_condition_heart_disease_or_stroke)
model3_df <- final_cleaned_data

# Create output directories if they don't exist
dir.create("logreg_output", showWarnings = FALSE)
dir.create("logreg_output/models", showWarnings = FALSE, recursive = TRUE)
dir.create("logreg_output/test_sets", showWarnings = FALSE, recursive = TRUE)
dir.create("logreg_output/coefficients", showWarnings = FALSE, recursive = TRUE)
dir.create("logreg_output/plots", showWarnings = FALSE, recursive = TRUE)


# Train and save regularized logistic regression models
train_and_save_model(model1_df, "has_a_high_blood_pressure",
                     model_path = "logreg_output/models/model_highbp.rds",
                     test_csv_path = "logreg_output/test_sets/test_highbp.csv")

train_and_save_model(model2_df, "has_diabetes",
                     model_path = "logreg_output/models/model_diabetes.rds",
                     test_csv_path = "logreg_output/test_sets/test_diabetes.csv")

train_and_save_model(model3_df, "cardiovascular_condition_heart_disease_or_stroke",
                     model_path = "logreg_output/models/model_cardio.rds",
                     test_csv_path = "logreg_output/test_sets/test_cardio.csv")

# Extract and plot coefficients
extract_logistic_coefficients("logreg_output/models/model_highbp.rds",
                              output_csv = "logreg_output/coefficients/logreg_coefficients_highbp.csv",
                              plot_title = "High BP - Top Logistic Features",
                              plot_path = "logreg_output/plots/logreg_plot_highbp.png")

extract_logistic_coefficients("logreg_output/models/model_diabetes.rds",
                              output_csv = "logreg_output/coefficients/logreg_coefficients_diabetes.csv",
                              plot_title = "Diabetes - Top Logistic Features",
                              plot_path = "logreg_output/plots/logreg_plot_diabetes.png")

extract_logistic_coefficients("logreg_output/models/model_cardio.rds",
                              output_csv = "logreg_output/coefficients/logreg_coefficients_cardio.csv",
                              plot_title = "Cardio Risk - Top Logistic Features",
                              plot_path = "logreg_output/plots/logreg_plot_cardio.png")

# -------------------------------
# Phase 7: GAM on Top Features
# -------------------------------

source("R/gam_top_features.R")

# Reload cleaned dataset
data("final_cleaned_data")
final_cleaned_data <- janitor::clean_names(final_cleaned_data)

# Prepare model-specific datasets
model1_df <- final_cleaned_data %>% select(-has_diabetes, -cardiovascular_condition_heart_disease_or_stroke)
model2_df <- final_cleaned_data %>% select(-cardiovascular_condition_heart_disease_or_stroke)
model3_df <- final_cleaned_data

# Run GAM models with top features
run_gam_on_top_features(model1_df, "has_a_high_blood_pressure")
run_gam_on_top_features(model2_df, "has_diabetes")
run_gam_on_top_features(model3_df, "cardiovascular_condition_heart_disease_or_stroke")



# -------------------------------
# Phase 8: ROC-AUC Evaluation
# -------------------------------

# Load required functions
source("R/generate_predictions.R")  # Ensures predictions exist
source("R/roc_auc.R")               # Plots ROC-AUC curves

# Step 1: Generate prediction CSVs if not already present
generate_all_logreg_predictions()

# Step 2: Define prediction file paths
pred_files <- list(
  "High BP"   = "logreg_output/predictions/predictions_highbp.csv",
  "Diabetes"  = "logreg_output/predictions/predictions_diabetes.csv",
  "Cardio"    = "logreg_output/predictions/predictions_cardio.csv"
)

# Step 3: Run ROC-AUC visualization and evaluation
run_roc_auc_phase(pred_files, output_dir = "logreg_output/roc_auc")


# -------------------------------
# Phase 9: Perceived vs Actual Health Analysis
# -------------------------------

source("R/perceived_health.R")

# Load cleaned dataset
data("final_cleaned_data")
final_cleaned_data <- janitor::clean_names(final_cleaned_data)

# Run perceived health analysis
run_perceived_health_analysis(
  df = final_cleaned_data,
  perceived_col = "perceived_health",
  output_dir = "perceived_health_plots/general"
)

# Run perceived mental health analysis
run_perceived_health_analysis(
  df = final_cleaned_data,
  perceived_col = "perceived_mental_health",
  output_dir = "perceived_health_plots/mental"
)

message("Phase 9 complete: Perceived health vs chronic condition analysis done.")



