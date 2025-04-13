#' Apply transformation logic to clean and merge categorical levels
#'
#' @param df Decoded CCHS dataset
#' @return Transformed dataframe
#' @export
transform_categorical <- function(df) {
  # Folder prep (create if not exists)
  if (!dir.exists("Statistics")) dir.create("Statistics", recursive = TRUE)
  if (!dir.exists("data-csv")) dir.create("data-csv", recursive = TRUE)

  # ---- Transformation Logic ----

  # a. Marital Status
  df$`Marital Status` <- gsub("Not stated", "Married/Common-law", df$`Marital Status`)

  # b. Considered suicide - lifetime
  df$`Considered suicide - lifetime` <- ifelse(
    df$`Considered suicide - lifetime` %in% c("Refusal", "Don’t know", "Don't know", "Not stated"),
    "Unknown", df$`Considered suicide - lifetime`
  )

  # c1 + c2. Suicide - last 12 months
  df$`Considered suicide - last 12 months` <- ifelse(
    df$`Considered suicide - last 12 months` == "Valid skip", "No", df$`Considered suicide - last 12 months`
  )
  df$`Considered suicide - last 12 months` <- ifelse(
    df$`Considered suicide - last 12 months` %in% c("Refusal", "Don’t know", "Don't know", "Not stated"),
    "Unknown", df$`Considered suicide - last 12 months`
  )

  # d1-d2. Smoking
  df$`Smoking status` <- gsub(
    "Lifetime abstainer \\(never smoked\\)|Experimental smoker \\(at least 1 cig, non-smoker now\\)",
    "Non-smoker (abstainer or experimental)", df$`Smoking status`
  )
  df$`Smoking status`[df$`Smoking status` == "Not stated"] <- "Unknown"

  # e1-e6. Cannabis dependence
  df$`Severity of Canabis Dependence` <- dplyr::recode(df$`Severity of Canabis Dependence`,
                                                       "Valid skip" = "No cannabis use",
                                                       "Not stated" = "Unknown"
  )

  dep_map <- c(
    "0" = "No dependence",
    "1" = "Mild dependence", "2" = "Mild dependence", "3" = "Mild dependence", "4" = "Mild dependence",
    "5" = "Moderate dependence", "6" = "Moderate dependence", "7" = "Moderate dependence",
    "8" = "Moderate dependence", "9" = "Moderate dependence", "10" = "Moderate dependence",
    "11" = "Severe dependence", "12" = "Severe dependence", "13" = "Severe dependence",
    "14" = "Severe dependence", "15" = "Severe dependence"
  )

  df$`Severity of Canabis Dependence` <- dplyr::recode(df$`Severity of Canabis Dependence`, !!!dep_map)

  # f1. Cannabis use
  df$`Used cannabis - 12 months` <- ifelse(
    df$`Used cannabis - 12 months` %in% c("Refusal", "Don’t know", "Don't know", "Not stated"),
    "Unknown", df$`Used cannabis - 12 months`
  )

  # g1. Care place
  df$`Usual place for immediate care for minor problem` <- ifelse(
    df$`Usual place for immediate care for minor problem` %in% c("Refusal", "Don’t know", "Don't know"),
    "Unknown", df$`Usual place for immediate care for minor problem`
  )

  # h1. Income
  df$`Total Household Income - All Sources` <- gsub("Not stated", "Unknown", df$`Total Household Income - All Sources`)

  # i1. Drop BMI 12–17
  df <- df[, !names(df) %in% "BMI age 12 to 17 (self-reported) - WHO classification"]

  # j1-n1. Replace 'Not stated' → 'Unknown' for selected columns
  to_unknown <- c(
    "BMI classification for adults aged 18 and over (adjusted) - international",
    "Pain health status",
    "Perceived health ",
    "Perceived mental health ",
    "Satisfaction with life in general ",
    "Had a seasonal flu shot (excluding H1N1) - lifetime",
    "Seasonal flu shot - last time",
    "Type of drinker",
    "Drank 5+ / 4+ drinks one occasion - frequency - 12 months",
    "Has sleep apnea",
    "Has high blood cholesterol / lipids",
    "High blood cholesterol / lipids - took medication - 1 month",
    "Has chronic fatigue syndrome",
    "Has a mood disorder (depression, bipolar, mania, dysthymia)",
    "Has an anxiety disorder (phobia, OCD, panic)",
    "Has respiratory chronic condition (asthma or COPD)",
    "Musculoskeletal condition (Arthritis, fibromyalgia, osteoporosis)",
    "High blood pressure - took medication - 1 month",
    "Has a high blood pressure",
    "Has diabetes",
    "Cardiovascular condition (Heart disease or stroke)"
  )

  for (col in to_unknown) {
    df[[col]] <- ifelse(
      df[[col]] %in% c("Refusal", "Don’t know", "Don't know", "Not stated"),
      "Unknown", df[[col]]
    )
  }

  return(df)
}
