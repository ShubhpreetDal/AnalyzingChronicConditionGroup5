#' Analyze Perceived vs Actual Health for Chronic Conditions
#'
#' Performs crosstabs, chi-square tests, and bar plots showing average condition prevalence
#' by perceived health levels (either general or mental health).
#'
#' @param df Cleaned dataset (already transformed)
#' @param perceived_col Column name: either "perceived_health" or "perceived_mental_health"
#' @param output_dir Directory to save plots and chi-square results
#' @export
run_perceived_health_analysis <- function(df, perceived_col, output_dir = "perceived_health_plots") {
  library(tidyverse)
  library(janitor)
  library(scales)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  chronic_conditions <- c(
    "has_a_high_blood_pressure",
    "has_diabetes",
    "cardiovascular_condition_heart_disease_or_stroke"
  )

  chi_results <- list()

  for (target in chronic_conditions) {
    message("Analyzing: ", perceived_col, " vs ", target)

    filtered_df <- df %>%
      filter(.data[[target]] %in% c("Yes", "No")) %>%
      mutate(
        !!target := if_else(.data[[target]] == "Yes", 1, 0)
      )

    # Label perceived categories with their sample size %
    level_props <- filtered_df %>%
      count(.data[[perceived_col]]) %>%
      mutate(
        label = paste0(.data[[perceived_col]], " (", percent(n / sum(n), accuracy = 1), ")")
      )

    filtered_df <- filtered_df %>%
      left_join(level_props, by = perceived_col) %>%
      mutate(perceived_label = factor(label, levels = unique(label)))

    # Plot
    p <- ggplot(filtered_df, aes(x = perceived_label, y = .data[[target]])) +
      stat_summary(fun = mean, geom = "bar", fill = "steelblue") +
      labs(
        title = paste("Prevalence of", target, "by", perceived_col),
        y = "Proportion with Condition",
        x = paste(perceived_col, "(% of total)")
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(
      filename = file.path(output_dir, paste0("barplot_", perceived_col, "_", target, ".png")),
      plot = p, width = 7, height = 5
    )

    # Chi-square test
    test_table <- table(df[[perceived_col]], df[[target]])
    chi_result <- chisq.test(test_table)

    chi_results[[target]] <- tibble(
      Target = target,
      Perceived = perceived_col,
      Chi_Square = round(chi_result$statistic, 3),
      p_value = round(chi_result$p.value, 5),
      Significant = ifelse(chi_result$p.value < 0.05, "Yes", "No")
    )

    message(sprintf("Chi-square Test (p = %.5f): %s",
                    chi_result$p.value,
                    ifelse(chi_result$p.value < 0.05, "SIGNIFICANT", "Not significant")))
  }

  # Save chi-square results
  bind_rows(chi_results) %>%
    write_csv(file.path(output_dir, paste0("chi_square_results_", perceived_col, ".csv")))
}
