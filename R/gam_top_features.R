#' Run GAM using top 7 categorical features (pre-selected) with random effect smooths
#'
#' @param df Modeling dataset (pre-filtered)
#' @param target Target variable name (snake_case)
#' @param output_dir Directory to save outputs (default = "model_results_gam")
#' @export
run_gam_on_top_features <- function(df, target, output_dir = "model_results_gam") {
  library(mgcv)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(yardstick)

  set.seed(42)

  if (!target %in% colnames(df)) stop("Target not found in dataset.")

  # Filter unknowns and ensure all categorical
  df <- df %>%
    filter(.data[[target]] != "Unknown") %>%
    mutate(across(everything(), as.factor))

  # ==== Define top features manually ====
  top_features <- list(
    has_a_high_blood_pressure = c(
      "age_group", "high_blood_cholesterol_lipids_took_medication_1_month",
      "high_blood_pressure_took_medication_1_month", "smoking_status",
      "severity_of_canabis_dependence", "perceived_health", "perceived_mental_health"
    ),
    has_diabetes = c(
      "age_group", "sex_at_birth", "perceived_health", "smoking_status",
      "musculoskeletal_condition_arthritis_fibromyalgia_osteoporosis", "perceived_mental_health", "used_cannabis_12_months"
    ),
    cardiovascular_condition_heart_disease_or_stroke = c(
      "age_group", "high_blood_cholesterol_lipids_took_medication_1_month",
      "smoking_status", "perceived_health", "bmi_classification_for_adults_aged_18_and_over_adjusted_international",
      "perceived_mental_health", "sex_at_birth"
    )
  )

  # Use hardcoded feature list
  features <- top_features[[target]]
  if (is.null(features)) stop("Top features not defined for this target.")

  df <- df %>% select(all_of(c(target, features)))

  # Output setup
  target_clean <- gsub("[^A-Za-z0-9]", "_", target)
  out_dir <- file.path(output_dir, target_clean)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Split
  split_idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
  train_data <- df[split_idx, ]
  test_data <- df[-split_idx, ]

  # GAM formula with random effects on all features
  smooth_terms <- paste0("s(", features, ", bs = 're')", collapse = " + ")
  gam_formula <- as.formula(paste(target, "~", smooth_terms))

  # Fit GAM
  gam_model <- gam(gam_formula, data = train_data, family = binomial)

  # Predict and evaluate
  gam_probs <- predict(gam_model, newdata = test_data, type = "response")
  gam_pred <- ifelse(gam_probs > 0.5, "Yes", "No") %>%
    factor(levels = levels(df[[target]]))

  result_df <- tibble(
    truth = test_data[[target]],
    prediction = gam_pred
  )

  cm <- conf_mat(result_df, truth = truth, estimate = prediction)
  write_csv(as_tibble(cm$table), file.path(out_dir, "gam_confusion_matrix.csv"))
  ggsave(file.path(out_dir, "gam_confusion_matrix.png"), autoplot(cm, type = "heatmap"), width = 5.5, height = 4)

  # Metrics
  metrics <- tibble(
    Target = target,
    Model = "GAM",
    Accuracy_Test = round(accuracy(result_df, truth, prediction) %>% pull(.estimate), 4),
    Precision_Yes = round(precision(result_df, truth, prediction, event_level = "second") %>% pull(.estimate), 4),
    Recall_Yes = round(recall(result_df, truth, prediction, event_level = "second") %>% pull(.estimate), 4),
    F1_Yes = round(f_meas(result_df, truth, prediction, event_level = "second") %>% pull(.estimate), 4),
    Precision_No = round(precision(result_df, truth, prediction, event_level = "first") %>% pull(.estimate), 4),
    Recall_No = round(recall(result_df, truth, prediction, event_level = "first") %>% pull(.estimate), 4),
    F1_No = round(f_meas(result_df, truth, prediction, event_level = "first") %>% pull(.estimate), 4),
    F1_Macro_Test = round(f_meas(result_df, truth, prediction, estimator = "macro") %>% pull(.estimate), 4)
  )
  write_csv(metrics, file.path(out_dir, "gam_metrics.csv"))

  # Smooth term plots
  plot_dir <- file.path(out_dir, "gam_smooths")
  dir.create(plot_dir, showWarnings = FALSE)
  pdf(file.path(plot_dir, "gam_term_plots.pdf"), width = 8, height = 6)
  plot(gam_model, pages = 1, residuals = TRUE, se = TRUE, rug = FALSE)
  dev.off()

  cat("GAM completed for:", target, "\n")
  cat("Metrics saved to:", file.path(out_dir, "gam_metrics.csv"), "\n")
}
