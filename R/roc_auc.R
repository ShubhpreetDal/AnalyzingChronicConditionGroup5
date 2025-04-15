#' Generate ROC-AUC Plots for Logistic Regression Models
#'
#' Reads model predictions, evaluates ROC-AUC, and saves plots (individual + combined).
#' Designed for models with binary "Yes"/"No" outcomes.
#'
#' @param pred_files Named list of paths to prediction CSVs
#' @param output_dir Directory where plots will be saved
#'
#' @return Combined ROC-AUC ggplot object (also saved as PNG)
#' @export
run_roc_auc_phase <- function(pred_files, output_dir = "model_results_roc") {
  library(tidymodels)
  library(dplyr)
  library(readr)
  library(ggplot2)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  get_roc_data <- function(csv_path, label) {
    df <- read_csv(csv_path, show_col_types = FALSE) %>%
      mutate(
        y_true = factor(y_true, levels = c("Yes", "No")),
        .pred_Yes = y_proba
      )

    roc <- roc_curve(df, truth = y_true, .pred_Yes) %>% mutate(model = label)
    auc <- roc_auc(df, truth = y_true, .pred_Yes) %>% pull(.estimate)

    list(roc = roc, auc = auc)
  }

  # Store all results
  results <- list()
  combined_roc <- tibble()

  for (model_name in names(pred_files)) {
    cat("Processing:", model_name, "\n")
    res <- get_roc_data(pred_files[[model_name]], model_name)

    # Save individual ROC plot
    p_individual <- ggplot(res$roc, aes(x = 1 - specificity, y = sensitivity)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      geom_abline(linetype = "dashed", color = "gray") +
      coord_equal() +
      labs(
        title = paste0(model_name, " – ROC Curve (AUC = ", round(res$auc, 3), ")"),
        x = "False Positive Rate",
        y = "True Positive Rate"
      ) +
      theme_bw()

    ggsave(file.path(output_dir, paste0("roc_auc_", tolower(gsub(" ", "_", model_name)), ".png")),
           p_individual, width = 6, height = 5)

    # Add to combined
    combined_roc <- bind_rows(combined_roc,
                              res$roc %>% mutate(model = paste0(model_name, " (AUC = ", round(res$auc, 3), ")")))

    # Store result
    results[[model_name]] <- res
  }

  # Combined ROC plot
  p_combined <- ggplot(combined_roc, aes(x = 1 - specificity, y = sensitivity, color = model)) +
    geom_line(linewidth = 1.2) +
    geom_abline(linetype = "dashed", color = "gray") +
    coord_equal() +
    labs(
      title = "ROC-AUC Curves for Logistic Models",
      x = "False Positive Rate", y = "True Positive Rate", color = "Model"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

  ggsave(file.path(output_dir, "roc_auc_all_models.png"), p_combined, width = 8, height = 6)
  cat("Saved combined ROC plot to:", file.path(output_dir, "roc_auc_all_models.png"), "\n")

  return(p_combined)
}
