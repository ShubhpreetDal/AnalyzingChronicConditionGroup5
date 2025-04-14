#' Extract and plot coefficients from regularized logistic regression
#'
#' @param model_path Path to saved .rds model
#' @param output_csv Output path for coefficient CSV
#' @param plot_title Title for coefficient plot
#' @param plot_path Output path for coefficient plot
#' @param top_n Number of top coefficients to display
#' @return Tibble with coefficients
#' @export
extract_logistic_coefficients <- function(model_path, output_csv,
                                          plot_title = NULL, plot_path = NULL, top_n = 7) {
  library(tidymodels)
  library(ggplot2)
  library(dplyr)
  library(readr)

  cat("\n===============================\n")
  cat(paste(" Extracting Coefficients from:", model_path, "\n"))

  model <- readRDS(model_path)
  glm_model <- extract_fit_parsnip(model)$fit
  best_lambda <- glm_model$lambda[1]
  coefs <- coef(glm_model, s = best_lambda)[-1, , drop = FALSE]

  coef_df <- tibble(
    feature = rownames(coefs),
    coefficient = as.numeric(coefs),
    abs_coefficient = abs(as.numeric(coefs)),
    odds_ratio = exp(as.numeric(coefs))
  ) %>% arrange(desc(abs_coefficient))

  write_csv(coef_df, output_csv)
  cat("Saved coefficient table to:", output_csv, "\n")

  top_features <- coef_df %>% slice_head(n = top_n)

  plot <- ggplot(top_features, aes(x = reorder(feature, coefficient), y = coefficient)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = plot_title %||% "Top Logistic Regression Features",
      x = "Feature",
      y = "Coefficient (Log Odds)"
    ) +
    theme_bw()
    #theme_minimal()

  print(plot)

  if (!is.null(plot_path)) {
    ggsave(plot_path, plot = plot, width = 8, height = 5)
    cat("Saved plot to:", plot_path, "\n")
  }

  return(coef_df)
}
