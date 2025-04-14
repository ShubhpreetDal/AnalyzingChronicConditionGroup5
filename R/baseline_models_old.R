#' Run classification models on a target variable and evaluate performance
#'
#' This function performs train-test splitting, one-hot encoding, downsampling,
#' cross-validation, final model fitting, and saves evaluation outputs.
#' It supports Logistic Regression and Decision Tree.
#'
#' @param df Dataframe containing predictor and target variables
#' @param target Name of the target variable (string, snake_case)
#' @param output_dir Directory to save CV metrics and test predictions (default = "model_results")
#'
#' @return Nothing. Saves evaluation outputs to CSV.
#' @export
run_baseline_models <- function(df, target, output_dir = "model_results") {
  library(tidymodels)
  library(themis)
  library(readr)
  library(dplyr)
  library(stringr)

  set.seed(42)

  # Validate that target column exists
  if (!target %in% colnames(df)) stop("Target column not found in dataframe")

  # Drop rows with "Unknown" in target, convert all columns to factors
  df <- df %>%
    filter(.data[[target]] != "Unknown") %>%
    mutate(across(everything(), as.factor))

  # Create target-specific output folder
  target_clean <- gsub("[^A-Za-z0-9]", "_", target)
  dir.create(file.path(output_dir, target_clean), recursive = TRUE, showWarnings = FALSE)

  # Train/test split
  split <- initial_split(df, strata = all_of(target), prop = 0.8)
  train_data <- training(split)
  test_data  <- testing(split)

  # Define preprocessing recipe: one-hot encode + downsample
  rec <- recipe(as.formula(paste(target, "~ .")), data = train_data) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_downsample(all_outcomes())

  # Define baseline models
  models <- list(
    logistic_reg = logistic_reg(mode = "classification") %>% set_engine("glm"),
    decision_tree = decision_tree(mode = "classification") %>% set_engine("rpart")
  )

  # Loop over each model
  for (model_name in names(models)) {
    model <- models[[model_name]]

    wf <- workflow() %>%
      add_model(model) %>%
      add_recipe(rec)

    # 5-fold cross-validation
    folds <- vfold_cv(train_data, v = 5, strata = all_of(target))
    res <- fit_resamples(
      wf,
      resamples = folds,
      metrics = metric_set(f_meas, accuracy, recall, precision),
      control = control_resamples(save_pred = TRUE)
    )

    # Save cross-validation metrics
    cv_metrics <- collect_metrics(res)
    cv_metrics$Model <- model_name
    cv_metrics$Target <- target
    write_csv(cv_metrics, file.path(output_dir, target_clean, paste0(model_name, "_cv_metrics.csv")))

    # Final model fit and test predictions
    final_fit <- fit(wf, data = train_data)
    predictions <- predict(final_fit, new_data = test_data) %>%
      bind_cols(test_data %>% select(all_of(target)))

    # Save predictions to CSV
    write_csv(predictions, file.path(output_dir, target_clean, paste0(model_name, "_predictions.csv")))

    # Print confusion matrix and metrics for class "Yes"
    cat("\nModel:", model_name, "→ Target:", target, "\n")
    print(conf_mat(predictions, truth = !!sym(target), estimate = .pred_class))

    cat("\nClass: Yes (positive class)\n")
    cat(sprintf("Precision (Yes): %.3f\n",
                precision(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "second") %>% pull(.estimate)))
    cat(sprintf("Recall    (Yes): %.3f\n",
                recall(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "second") %>% pull(.estimate)))
    cat(sprintf("F1 Score  (Yes): %.3f\n",
                f_meas(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "second") %>% pull(.estimate)))

    cat("\nOverall Accuracy: ",
        round(accuracy(predictions, truth = !!sym(target), estimate = .pred_class) %>% pull(.estimate), 3), "\n")
  }

  cat("\n Results saved in:", file.path(output_dir, target_clean), "\n")
}
