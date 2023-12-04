# lets define a function with this process
# this function will take in a df, xvs and yv and return a trained model
random_forest_fx <- function(xv, yv, df) {
  library(tidyverse)
  library(tidymodels)
  library(vcd)
  set.seed(1234)

########################################################
  # remove na's
  # remove every column which is more than 25% NA
  df <- df[, colMeans(is.na(df)) < 0.25]
  df <- df %>% drop_na()
########################################################

#######################################################
  # lets split the data into training and testing
  data_split <- initial_split(df, prop = .8)
  training_data <- training(data_split)
  testing_data <- testing(data_split)
#######################################################


#######################################################
  model_formula <- reformulate(termlabels = xv, response = yv)
  # Start the recipe
  model_recipe <- recipe(model_formula, data = training_data)
  
  # Conditionally update role if PUF_CASE_ID is in the data
  if ("PUF_CASE_ID" %in% names(df)) {
    model_recipe <- model_recipe %>%
      update_role(PUF_CASE_ID, new_role = "id")
  }
  
  # Continue with the rest of the recipe
  model_recipe <- model_recipe %>%
    # Apply necessary recipe steps here...
    # step_dummy(all_nominal(), -all_outcomes()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors(), -all_nominal())
#######################################################


#######################################################
  # lets run a random forest model
  rf_model <-
    rand_forest(trees = 10) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")

  rf_workflow <-
    workflow() %>%
    add_model(rf_model) %>%
    add_recipe(model_recipe)

  rf_fit <- rf_workflow %>% fit(data = training_data)
#######################################################


#######################################################
  rfpredict <- rf_fit %>% predict(new_data = training_data) %>% bind_cols(training_data)
  rfpredict <- rf_fit %>% predict(new_data = training_data, type= "prob") %>% bind_cols(rfpredict)
  metrics(rfpredict, yv, .pred_class)
#######################################################


#######################################################
  rftestpredict <- rf_fit %>% predict(new_data = testing_data) %>% bind_cols(testing_data)
  rftestpredict <- rf_fit %>% predict(new_data = testing_data, type= "prob") %>% bind_cols(rftestpredict)
  performance_stats <- metrics(rftestpredict, yv, .pred_class)
#######################################################


  return(performance_stats)
}