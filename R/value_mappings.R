#' Value mappings for decoding categorical variables in CCHS
#' @export
value_mappings <- list(
  # Demographics
  "Age Group" = c("1" = "12-17 years", "2" = "18 to 34 years", "3" = "35 to 49 years", "4" = "50 to 64 years", "5" = "65 and older"),
  "Sex at Birth" = c("1" = "Male", "2" = "Female"),
  "Marital Status" = c("1" = "Married/Common-law", "2" = "Widowed/Divorced/Separated/Single, never married", "6" = "Valid skip (under 18)", "9" = "Not stated"),

  # Suicide
  "Considered suicide - lifetime" = c("1" = "Yes", "2" = "No", "6" = "Valid skip", "7" = "Don’t know", "8" = "Refusal", "9" = "Not stated"),
  "Considered suicide - last 12 months" = c("1" = "Yes", "2" = "No", "6" = "Valid skip", "7" = "Don’t know", "8" = "Refusal", "9" = "Not stated"),

  # Smoking
  "Smoking status" = c("1" = "Current daily smoker", "2" = "Current occasional smoker", "3" = "Former daily smoker (non-smoker now)", "4" = "Former occasional smoker (non-smoker now)", "5" = "Experimental smoker (at least 1 cig, non-smoker now)", "6" = "Lifetime abstainer (never smoked)", "99" = "Not stated"),

  # Cannabis
  "Severity of Canabis Dependence" = setNames(as.character(0:15), as.character(0:15)) |>
    c("96" = "Valid skip", "99" = "Not stated"),
  "Used cannabis - 12 months" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal", "9" = "Not stated"),

  # Primary Health care
  "Usual place for immediate care for minor problem" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal"),

  # Income
  "Total Household Income - All Sources" = c("1" = "No income or less than $20,000", "2" = "$20,000 to $39,999", "3" = "$40,000 to $59,999", "4" = "$60,000 to $79,999", "5" = "$80,000 or more", "9" = "Not stated"),

  # BMI
  "BMI age 12 to 17 (self-reported) - WHO classification" = c("1" = "Thinness/Normal", "2" = "Overweight/Obese", "6" = "Valid skip", "9" = "Not stated"),
  "BMI classification for adults aged 18 and over (adjusted) - international" = c("1" = "Underweight/ Normal weight", "2" = "Overweight / Obese - Class I, II, III", "6" = "Valid skip", "9" = "Not stated"),

  # Health Utility
  "Pain health status" = c("1" = "No usual pain or discomfort", "2" = "Has usual pain or discomfort", "9" = "Not stated"),

  # General Health
  "Perceived health " = c("0" = "Poor", "1" = "Fair", "2" = "Good", "3" = "Very good", "4" = "Excellent", "9" = "Not stated"),
  "Perceived mental health " = c("0" = "Poor", "1" = "Fair", "2" = "Good", "3" = "Very good", "4" = "Excellent", "9" = "Not stated"),
  "Satisfaction with life in general " = c("1" = "Very Satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Very Dissatisfied", "9" = "Not stated"),

  # Flu
  "Had a seasonal flu shot (excluding H1N1) - lifetime" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal", "9" = "Not stated"),
  "Seasonal flu shot - last time" = c("1" = "Less than 1 year ago", "2" = "1 year to less than 2 years ago", "3" = "2 years ago or more", "6" = "Valid skip", "7" = "Don’t know", "8" = "Refusal", "9" = "Not stated"),

  # Alcohol
  "Type of drinker" = c("1" = "Regular drinker", "2" = "Occasional drinker", "3" = "Did not drink in the last 12 months", "9" = "Not stated"),
  "Drank 5+ / 4+ drinks one occasion - frequency - 12 months" = c(
    "1" = "Never", "2" = "Less than once a month", "3" = "Once a month", "4" = "2-3 times a month", "5" = "Once a week", "6" = "More than once a week",
    "96" = "Valid skip", "97" = "Don’t know", "98" = "Refusal", "99" = "Not stated"),

  # Chronic Conditions
  "Has sleep apnea" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal"),
  "Has high blood cholesterol / lipids" = c("1" = "Yes", "2" = "No", "6" = "Valid skip", "7" = "Don’t know", "8" = "Refusal"),
  "High blood cholesterol / lipids - took medication - 1 month" = c("1" = "Yes", "2" = "No", "6" = "Valid skip", "7" = "Don’t know", "8" = "Refusal"),
  "Has chronic fatigue syndrome" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal"),
  "Has a mood disorder (depression, bipolar, mania, dysthymia)" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal"),
  "Has an anxiety disorder (phobia, OCD, panic)" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal"),
  "Has respiratory chronic condition (asthma or COPD)" = c("1" = "Yes", "2" = "No", "9" = "Not stated"),
  "Musculoskeletal condition (Arthritis, fibromyalgia, osteoporosis)" = c("1" = "Yes", "2" = "No", "6" = "Valid skip", "9" = "Not stated"),

  # Targets
  "High blood pressure - took medication - 1 month" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal"),
  "Has a high blood pressure" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal"),
  "Has diabetes" = c("1" = "Yes", "2" = "No", "7" = "Don’t know", "8" = "Refusal", "9" = "Not stated"),
  "Cardiovascular condition (Heart disease or stroke)" = c("1" = "Yes", "2" = "No", "9" = "Not stated")
)
