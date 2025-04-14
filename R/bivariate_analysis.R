#' Run bivariate analysis: chi-square test and stacked plots for all features vs. target
#'
#' @param df A cleaned dataframe
#' @param target Name of the target variable (string)
#' @param output_dir Folder where plots and chi-square CSV will be saved
#' @export
run_bivariate_analysis <- function(df, target, output_dir = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")

  # Create clean output directory name
  clean_name <- gsub("[^A-Za-z0-9_]", "_", target)
  if (is.null(output_dir)) {
    output_dir <- file.path("bivariate_plots", clean_name)
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  chi_results <- data.frame(Feature = character(), `p-value` = numeric(), stringsAsFactors = FALSE)

  for (col in names(df)) {
    if (col == target) next

    tbl <- table(df[[col]], df[[target]])

    # Chi-square test
    result <- tryCatch({
      test <- chisq.test(tbl)
      pval <- round(test$p.value, 4)
    }, error = function(e) {
      pval <- NA
    })

    chi_results <- rbind(chi_results, data.frame(Feature = col, `p-value` = result))

    # Stacked bar plot
    prop_tbl <- prop.table(tbl, margin = 1)
    plot_df <- as.data.frame(prop_tbl)
    names(plot_df) <- c("Feature_Level", "Target_Level", "Proportion")

    # Add population share to label
    level_props <- prop.table(table(df[[col]])) * 100
    plot_df$Feature_Level <- paste0(plot_df$Feature_Level, " (", round(level_props[plot_df$Feature_Level], 1), "%)")

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Feature_Level, y = Proportion, fill = Target_Level)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste(col, "vs", target),
        x = col,
        y = "Proportion"
      ) +
      ggplot2::theme_bw()
      #ggplot2::theme_minimal()

    safe_colname <- gsub("[^A-Za-z0-9_]", "_", col)
    plot_path <- file.path(output_dir, paste0(safe_colname, "_vs_", clean_name, ".png"))
    ggplot2::ggsave(plot_path, plot = p, width = 9, height = 5)
  }

  # Save chi-square summary
  csv_path <- file.path(output_dir, paste0("bivariate_chi_square_vs_", clean_name, ".csv"))
  write.csv(chi_results, csv_path, row.names = FALSE)

  message("Bivariate analysis completed for: ", target)
}
