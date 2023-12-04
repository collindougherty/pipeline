linear_regression_fx <- function(xv, yv, df) {
  library(tidyverse)
  library(tidymodels)
  
  set.seed(1234) # Setting seed for reproducibility
  
  # Remove columns with more than 25% NA values and drop remaining NAs
  df <- df[, colMeans(is.na(df)) < 0.25]
  df <- df %>% drop_na()
  
  # Split the data into training and testing
  data_split <- initial_split(df, prop = .8)
  training_data <- training(data_split)
  testing_data <- testing(data_split)
  
  # Create a model formula
  model_formula <- reformulate(termlabels = xv, response = yv)
  
  # Start the recipe
  model_recipe <- recipe(model_formula, data = training_data) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors(), -all_nominal())
  
  # Define the linear regression model
  lr_model <- linear_reg() %>%
    set_engine("lm")
  
  # Create the workflow
  lr_workflow <- workflow() %>%
    add_model(lr_model) %>%
    add_recipe(model_recipe)
  
  # Fit the model
  lr_fit <- lr_workflow %>% fit(data = training_data)
  
  # Predictions and metrics on training data
  lr_predict_train <- lr_fit %>% predict(new_data = training_data) %>% bind_cols(training_data)
  metrics(lr_predict_train, yv, .pred_class)
  
  # Predictions and metrics on testing data
  lr_predict_test <- lr_fit %>% predict(new_data = testing_data) %>% bind_cols(testing_data)
  performance_stats <- metrics(lr_predict_test, yv, .pred_class)
  
  return(performance_stats)
}
