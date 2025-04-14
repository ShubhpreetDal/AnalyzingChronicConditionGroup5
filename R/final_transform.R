#' Final transformations for sparse levels and missing category cleanup
#'
#' @param df Dataframe after initial transformation
#' @return Final cleaned dataframe
#' @export
final_transform <- function(df) {

  # --- a1 & a2. Merge cannabis dependence levels ---
  df$`Severity of Canabis Dependence` <- dplyr::recode(df$`Severity of Canabis Dependence`,
                                                       "Mild dependence" = "Takes cannabis & dependent on it",
                                                       "Moderate dependence" = "Takes cannabis & dependent on it",
                                                       "Severe dependence" = "Takes cannabis & dependent on it",
                                                       "No dependence" = "Takes cannabis but no dependence"
  )

  # --- b1–n1. Replace 'Unknown' with mode values (excluding life satisfaction) ---
  to_mode <- c(
    "Usual place for immediate care for minor problem",
    "Pain health status",
    "Perceived health ",
    "Type of drinker",
    "Drank 5+ / 4+ drinks one occasion - frequency - 12 months",
    "Has sleep apnea",
    "High blood cholesterol / lipids - took medication - 1 month",
    "Has chronic fatigue syndrome",
    "Has a mood disorder (depression, bipolar, mania, dysthymia)",
    "Has an anxiety disorder (phobia, OCD, panic)",
    "High blood pressure - took medication - 1 month",
    "Has a high blood pressure"
  )

  for (col in to_mode) {
    mode_val <- names(which.max(table(df[[col]])))
    df[[col]] <- ifelse(df[[col]] == "Unknown", mode_val, df[[col]])
  }

  # --- e1. Merge 'Very Dissatisfied' into 'Dissatisfied' ---
  df$`Satisfaction with life in general ` <- dplyr::recode(
    df$`Satisfaction with life in general `,
    "Very Dissatisfied" = "Dissatisfied"
  )

  return(df)
}
