#' Train and save final logistic regression model (L2 regularized with glmnet)
#'
#' @param df A pre-loaded dataframe
#' @param target_variable Target column name (string)
#' @param model_path Output path to save trained model as .rds
#' @param test_csv_path Output path to save test set
#' @param recipe_path Optional: path to save recipe used
#' @export
train_and_save_model <- function(df, target_variable,
                                 model_path, test_csv_path, recipe_path = NULL) {
  library(tidymodels)
  library(themis)
  library(readr)

  set.seed(42)

  df <- df %>%
    filter(.data[[target_variable]] != "Unknown") %>%
    mutate(across(everything(), as.factor))

  split <- initial_split(df, strata = all_of(target_variable), prop = 0.8)
  train_data <- training(split)
  test_data  <- testing(split)

  recipe <- recipe(as.formula(paste(target_variable, "~ .")), data = train_data) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_downsample(all_outcomes())

  model <- logistic_reg(mode = "classification", penalty = 0.01, mixture = 0) %>%
    set_engine("glmnet")

  wf <- workflow() %>%
    add_model(model) %>%
    add_recipe(recipe)

  final_model <- fit(wf, data = train_data)

  saveRDS(final_model, model_path)
  write_csv(test_data, test_csv_path)

  if (!is.null(recipe_path)) {
    saveRDS(recipe, recipe_path)
  }

  cat("Model and test set saved.\n")
}
