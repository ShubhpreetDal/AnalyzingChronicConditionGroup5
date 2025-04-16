#' Generate prediction CSVs from saved logistic regression models
#'
#' This utility ensures predictions are available for all targets.
#' Run this before ROC-AUC evaluation if prediction CSVs are missing.
#'
#' @param base_path Optional prefix for relative paths. Use "../" in vignette context.
#' @export
generate_all_logreg_predictions <- function(base_path = "") {
  library(tidymodels)
  library(readr)
  library(dplyr)

  output_dir <- file.path(base_path, "logreg_output/predictions")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  targets <- list(
    list(
      name = "High BP",
      target = "has_a_high_blood_pressure",
      model_path = file.path(base_path, "logreg_output/models/model_highbp.rds"),
      test_path = file.path(base_path, "logreg_output/test_sets/test_highbp.csv"),
      output_path = file.path(base_path, "logreg_output/predictions/predictions_highbp.csv")
    ),
    list(
      name = "Diabetes",
      target = "has_diabetes",
      model_path = file.path(base_path, "logreg_output/models/model_diabetes.rds"),
      test_path = file.path(base_path, "logreg_output/test_sets/test_diabetes.csv"),
      output_path = file.path(base_path, "logreg_output/predictions/predictions_diabetes.csv")
    ),
    list(
      name = "Cardio",
      target = "cardiovascular_condition_heart_disease_or_stroke",
      model_path = file.path(base_path, "logreg_output/models/model_cardio.rds"),
      test_path = file.path(base_path, "logreg_output/test_sets/test_cardio.csv"),
      output_path = file.path(base_path, "logreg_output/predictions/predictions_cardio.csv")
    )
  )

  for (t in targets) {
    if (!file.exists(t$output_path)) {
      cat("Generating predictions for:", t$name, "\n")
      df <- read_csv(t$test_path, show_col_types = FALSE) %>%
        filter(.data[[t$target]] %in% c("Yes", "No")) %>%
        mutate(across(everything(), as.factor))

      model <- readRDS(t$model_path)
      probs <- predict(model, new_data = df %>% select(-all_of(t$target)), type = "prob")$.pred_Yes

      results <- tibble(
        y_true = df[[t$target]],
        y_proba = probs
      )

      write_csv(results, t$output_path)
      cat("Saved:", t$output_path, "\n")
    } else {
      cat(" Already exists:", t$output_path, "\n")
    }
  }

  cat("\nAll predictions ready.\n")
}
