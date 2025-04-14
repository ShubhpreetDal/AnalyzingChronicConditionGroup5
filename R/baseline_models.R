#' Run Logistic Regression and Decision Tree models and save detailed evaluation
#'
#' @param df A dataframe with predictors and target
#' @param target Target variable name (snake_case)
#' @param output_dir Output directory (default: "model_results")
#' @export
run_baseline_models <- function(df, target, output_dir = "model_results") {
  library(tidymodels)
  library(themis)
  library(readr)
  library(ggplot2)
  library(dplyr)

  set.seed(42)

  # Sanity check
  if (!target %in% colnames(df)) stop("Target column not found.")

  # Preprocess
  df <- df %>%
    filter(.data[[target]] != "Unknown") %>%
    mutate(across(everything(), as.factor))

  # Create folder
  target_clean <- gsub("[^A-Za-z0-9]", "_", target)
  out_dir <- file.path(output_dir, target_clean)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Split data
  split <- initial_split(df, strata = all_of(target), prop = 0.8)
  train_data <- training(split)
  test_data  <- testing(split)

  # Recipe
  rec <- recipe(as.formula(paste(target, "~ .")), data = train_data) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_downsample(all_outcomes())

  models <- list(
    logistic_reg = logistic_reg(mode = "classification") %>% set_engine("glm"),
    decision_tree = decision_tree(mode = "classification") %>% set_engine("rpart")
  )

  for (model_name in names(models)) {
    model <- models[[model_name]]
    wf <- workflow() %>% add_model(model) %>% add_recipe(rec)

    # CV
    folds <- vfold_cv(train_data, v = 5, strata = all_of(target))
    res <- fit_resamples(
      wf,
      resamples = folds,
      metrics = metric_set(accuracy, f_meas, recall, precision),
      control = control_resamples(save_pred = TRUE)
    )
    cv_metrics <- collect_metrics(res)
    acc_cv_mean <- cv_metrics %>% filter(.metric == "accuracy") %>% pull(mean)
    acc_cv_std  <- cv_metrics %>% filter(.metric == "accuracy") %>% pull(std_err)
    f1_cv_mean  <- cv_metrics %>% filter(.metric == "f_meas") %>% pull(mean)
    f1_cv_std   <- cv_metrics %>% filter(.metric == "f_meas") %>% pull(std_err)

    # Final model
    final_fit <- fit(wf, data = train_data)
    predictions <- predict(final_fit, new_data = test_data) %>%
      bind_cols(test_data %>% select(all_of(target)))

    # Conf matrix + metrics
    cm <- conf_mat(predictions, truth = !!sym(target), estimate = .pred_class)
    cm_table <- as_tibble(cm$table)
    write_csv(cm_table, file.path(out_dir, paste0(model_name, "_confusion_matrix.csv")))

    # Plot
    p <- autoplot(cm, type = "heatmap") + ggplot2::ggtitle(paste("Confusion Matrix –", model_name))
    ggsave(file.path(out_dir, paste0(model_name, "_confusion_matrix.png")), plot = p, width = 5.5, height = 4)

    # Test set metrics
    acc_test <- accuracy(predictions, truth = !!sym(target), estimate = .pred_class) %>% pull(.estimate)

    prec_yes <- precision(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "second") %>% pull(.estimate)
    rec_yes  <- recall(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "second") %>% pull(.estimate)
    f1_yes   <- f_meas(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "second") %>% pull(.estimate)

    prec_no <- precision(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "first") %>% pull(.estimate)
    rec_no  <- recall(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "first") %>% pull(.estimate)
    f1_no   <- f_meas(predictions, truth = !!sym(target), estimate = .pred_class, event_level = "first") %>% pull(.estimate)

    f1_macro <- f_meas(predictions, truth = !!sym(target), estimate = .pred_class, estimator = "macro") %>% pull(.estimate)

    # Save detailed evaluation row
    row <- tibble(
      Target = target,
      Model = model_name,
      Accuracy_Mean_CV = round(acc_cv_mean, 4),
      Accuracy_Std_CV = round(acc_cv_std, 4),
      F1_Macro_Mean_CV = round(f1_cv_mean, 4),
      F1_Macro_Std_CV = round(f1_cv_std, 4),
      Accuracy_Test = round(acc_test, 4),
      Precision_Yes = round(prec_yes, 4),
      Recall_Yes = round(rec_yes, 4),
      F1_Yes = round(f1_yes, 4),
      Precision_No = round(prec_no, 4),
      Recall_No = round(rec_no, 4),
      F1_No = round(f1_no, 4),
      F1_Macro_Test = round(f1_macro, 4)
    )

    # Append or create file
    out_path <- file.path(out_dir, "combined_metrics.csv")
    if (file.exists(out_path)) {
      existing <- read_csv(out_path, show_col_types = FALSE)
      combined <- bind_rows(existing, row)
    } else {
      combined <- row
    }
    write_csv(combined, out_path)

    # Save predictions too
    write_csv(predictions, file.path(out_dir, paste0(model_name, "_predictions.csv")))

    # Print summary
    cat("\nModel:", model_name, "→ Target:", target, "\n")
    print(cm)
    cat(" Metrics appended to:", out_path, "\n")
  }
}
