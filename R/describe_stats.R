#' Generate descriptive statistics for all categorical variables
#'
#' @param df A data.frame with categorical columns
#' @return A data.frame with frequencies, proportions, and first mode
#' @export
describe_categorical <- function(df) {
  desc_stats <- data.frame(
    `S. No.` = integer(),
    Feature = character(),
    Level = character(),
    Frequency = integer(),
    `Proportion (%)` = numeric(),
    `First Mode` = character(),
    stringsAsFactors = FALSE
  )

  serial_no <- 1
  for (col in colnames(df)) {
    freq <- table(df[[col]], useNA = "ifany")
    prop <- prop.table(freq) * 100
    mode_val <- if (length(names(which.max(freq))) > 0) names(which.max(freq))[1] else NA

    for (level in names(freq)) {
      desc_stats <- rbind(desc_stats, data.frame(
        `S. No.` = serial_no,
        Feature = col,
        Level = level,
        Frequency = freq[[level]],
        `Proportion (%)` = round(prop[[level]], 2),
        `First Mode` = if (level == mode_val) mode_val else "",
        stringsAsFactors = FALSE
      ))
    }

    serial_no <- serial_no + 1
  }

  return(desc_stats)
}
