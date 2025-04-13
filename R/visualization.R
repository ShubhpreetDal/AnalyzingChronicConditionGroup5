#' Generate bar plots for all categorical variables in a dataset
#'
#' This function takes a data frame and generates bar plots for each column,
#' saving the plots in the specified output directory.
#'
#' @param df A data frame (categorical variables recommended)
#' @param output_dir Path to the folder where bar plots will be saved
#' @export
generate_barplots <- function(df, output_dir = "Plots") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  for (colname in names(df)) {
    # Dynamically refer to column using tidy evaluation
    p <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(colname))) +
      ggplot2::geom_bar(fill = "steelblue") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste("Distribution of", colname),
        x = "Category",
        y = "Count"
      ) +
      ggplot2::theme_bw()  # <--- white background

    # Clean file name by removing special characters
    safe_name <- gsub("[^A-Za-z0-9_]+", "_", colname)
    plot_path <- file.path(output_dir, paste0(safe_name, "_barplot.png"))
    ggplot2::ggsave(plot_path, plot = p, width = 8, height = 5)
  }

  message("Bar plots saved in: ", output_dir)
}
