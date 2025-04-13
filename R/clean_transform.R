#' Load and Subset CCHS Data
#'
#' Loads the raw CCHS CSV file, selects relevant columns, and renames them for readability.
#'
#' @param file_path Path to the CCHS dataset (PUMF) CSV file.
#' @return A data.frame with selected and renamed columns.
#' @export
load_cchs_data <- function(file_path) {
  selected_columns <- c(
    "DHHGAGE" = "Age Group",
    "DHH_SEX" = "Sex at Birth",
    "DHHGMS" = "Marital Status",
    "SUI_005" = "Considered suicide - lifetime",
    "SUI_010" = "Considered suicide - last 12 months",
    "SMKDVSTY" = "Smoking status",
    "SDSDVTOT" = "Severity of Canabis Dependence",
    "CAN_015" = "Used cannabis - 12 months",
    "PHC_005" = "Usual place for immediate care for minor problem",
    "INCDGHH" = "Total Household Income - All Sources",
    "HWTDGWHO" = "BMI age 12 to 17 (self-reported) - WHO classification",
    "HWTDGBCC" = "BMI classification for adults aged 18 and over (adjusted) - international",
    "HUIDGPAD" = "Pain health status",
    "GENDVHDI" = "Perceived health ",
    "GENDVMHI" = "Perceived mental health ",
    "GENDVSWL" = "Satisfaction with life in general ",
    "FLU_005" = "Had a seasonal flu shot (excluding H1N1) - lifetime",
    "FLU_010" = "Seasonal flu shot - last time",
    "ALCDVTTM" = "Type of drinker",
    "ALC_020" = "Drank 5+ / 4+ drinks one occasion - frequency - 12 months",
    "CCC_035" = "Has sleep apnea",
    "CCC_075" = "Has high blood cholesterol / lipids",
    "CCC_080" = "High blood cholesterol / lipids - took medication - 1 month",
    "CCC_185" = "Has chronic fatigue syndrome",
    "CCC_195" = "Has a mood disorder (depression, bipolar, mania, dysthymia)",
    "CCC_200" = "Has an anxiety disorder (phobia, OCD, panic)",
    "CCCDGRSP" = "Has respiratory chronic condition (asthma or COPD)",
    "CCCDGSKL" = "Musculoskeletal condition (Arthritis, fibromyalgia, osteoporosis)",
    "CCC_070" = "High blood pressure - took medication - 1 month",
    "CCC_065" = "Has a high blood pressure",
    "CCC_095" = "Has diabetes",
    "CCCDGCAR" = "Cardiovascular condition (Heart disease or stroke)"
  )

  df <- read.csv(file_path)
  missing_cols <- setdiff(names(selected_columns), names(df))
  if (length(missing_cols) > 0) {
    warning("Missing columns in dataset: ", paste(missing_cols, collapse = ", "))
  }

  df_subset <- df[, names(selected_columns)]
  colnames(df_subset) <- unname(selected_columns)
  return(df_subset)
}


#' Decode CCHS categorical values
#'
#' Applies value mapping to raw CCHS codes for interpretability.
#'
#' @param df A dataframe containing raw values (before mapping).
#' @param mappings A named list of value mappings per column.
#' @return A dataframe with decoded values.
#' @export
decode_cchs_values <- function(df, mappings) {
  for (col in names(mappings)) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(mappings[[col]][as.character(df[[col]])])
    }
  }
  return(df)
}

#' Analyze target variable prevalence in age group 12–17
#'
#' Evaluates whether there are sufficient positive cases for target variables in the 12–17 age group.
#'
#' @param df Decoded dataset
#' @param target_vars A character vector of target variable column names
#' @return A summary dataframe with positive counts and proportions for 12–17 age group
#' @export
analyze_underage_targets <- function(
    df,
    target_vars = c(
      "Has a high blood pressure",
      "Has diabetes",
      "Cardiovascular condition (Heart disease or stroke)"
    )
) {
  underage_df <- df[df$`Age Group` == "12-17 years", ]

  results <- data.frame(
    `Target Variable` = character(),
    `Positive Count (Age 12–17)` = integer(),
    `Total Count (Age 12–17)` = integer(),
    `Proportion (%)` = numeric(),
    stringsAsFactors = FALSE
  )

  for (target in target_vars) {
    total <- sum(!is.na(underage_df[[target]]))
    positives <- sum(underage_df[[target]] == "Yes", na.rm = TRUE)
    prop <- if (total > 0) round(100 * positives / total, 2) else NA

    results <- rbind(results, data.frame(
      `Target Variable` = target,
      `Positive Count (Age 12–17)` = positives,
      `Total Count (Age 12–17)` = total,
      `Proportion (%)` = prop,
      stringsAsFactors = FALSE
    ))
  }

  print("Positive Cases in Age Group 12–17:")
  print(results, row.names = FALSE)
  return(results)
}

#' Drop respondents aged 12–17 due to low target case counts
#'
#' @param df Decoded dataset
#' @return Filtered dataset excluding "12-17 years"
#' @export
drop_underage_group <- function(df) {
  df[df$`Age Group` != "12-17 years", ]
}
