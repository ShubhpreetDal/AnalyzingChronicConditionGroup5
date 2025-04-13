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

