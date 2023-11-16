library(tidyverse)
library(tidymodels)
library(vcd)
set.seed(1234)

# remove every column which is more than 25% NA
# df <- df[, colMeans(is.na(df)) < 0.25]

# lets define a function with this process
# this function will take in a df, xvs and yv and return a trained model
random_forest_fx <- function(xv, yv, df) {

########################################################
  # remove na's
  df <- df %>% drop_na()
########################################################

#######################################################
  # lets split the data into training and testing
  data_split <- initial_split(df, prop = .8)
  training_data <- training(data_split)
  testing_data <- testing(data_split)
#######################################################


#######################################################
  model_recipe <-
    recipe(yv ~ xv, data = training_data) %>%
    # if PUF_CASE_ID is in the data, mark it as id
    update_role(PUF_CASE_ID, new_role = "id") %>%
    # Apply step_dummy() for factor variables
    # step_dummy(all_nominal_predictors()) %>%
    # remove all zero variance predictors
    step_zv(all_predictors()) %>%
    # Normalize only numeric predictors
    step_normalize(all_numeric_predictors())
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
print(metrics(rftestpredict, yv, .pred_class))
tb <- metrics(rftestpredict, yv, .pred_class)
#######################################################


#######################################################
# accuracy <- tb[1, 3]
# if (accuracy == 1) {
#   rf_model <- workflows::extract_fit_engine(rf_fit)
#   importance <- vip::vip(rf_model)
#   # importance is a plot, not a dataframe
#   # so we need to extract the dataframe from the plot
#   importance_df <- importance$data
#   # sort the dataframe by importance
#   importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
#   # get the 10 most important variables along with their importance values
#   most_important_vars <- importance_df[1:10, c("Variable", "Importance")]
#   message("The model's accuracy is 1, which almost certainly indicates selection of variables with perfect correlation to what you are trying to predict. Variables with extremely high importance relative to the rest are likely culprits. Consider examining variables with outlier high importance values:")
#   print(most_important_vars)
#   # also show the plot
#   #print(importance)
#   # update the plot to have text explaining what the user should do
#   # this is a hacky way to do it, but it works
#   # first, get the plot
#   plot <- importance$plot
#   # then, add the text
#   plot <- plot + ggplot2::annotate("text", x = 0, y = 0, label = "The model's accuracy is 1, which almost certainly indicates selection of variables with perfect correlation to what you are trying to predict. Variables with extremely high importance relative to the rest are likely culprits. Consider examining variables with outlier high importance values:", hjust = 0, vjust = 0, size = 3)
#   # then, update the plot
#   importance$plot <- plot
#   # then, print the plot
#   print(importance)
# }
#######################################################


#######################################################
# lets plot the ROC curve
# first, get the predictions
rfpred <- rf_fit %>% predict(new_data = testing_data, type = "prob") %>% bind_cols(testing_data)
# then, plot the ROC curve
roc <- roc_curve(rfpred, yv, .pred_1)
#######################################################


#######################################################
# lets plot the precision recall curve
# first, get the predictions
rfpred <- rf_fit %>% predict(new_data = testing_data, type = "prob") %>% bind_cols(testing_data)
# then, plot the precision recall curve
pr <- precision_recall_curve(rfpred, yv, .pred_1)
#######################################################


#######################################################
# lets plot the lift curve
# first, get the predictions
rfpred <- rf_fit %>% predict(new_data = testing_data, type = "prob") %>% bind_cols(testing_data)
# then, plot the lift curve
lift <- lift_curve(rfpred, yv, .pred_1)
#######################################################


#######################################################
# lets plot the gains curve
# first, get the predictions
rfpred <- rf_fit %>% predict(new_data = testing_data, type = "prob") %>% bind_cols(testing_data)
# then, plot the gains curve
gains <- gains_curve(rfpred, yv, .pred_1)
#######################################################


#######################################################
# lets plot the cumulative gains curve
# first, get the predictions
rfpred <- rf_fit %>% predict(new_data = testing_data, type = "prob") %>% bind_cols(testing_data)
# then, plot the cumulative gains curve
cumgains <- gains_curve(rfpred, yv, .pred_1, cumulative = TRUE)
#######################################################


#######################################################
# lets get a table of the confusion matrix and performance stats
# first, get the predictions
rfpred <- rf_fit %>% predict(new_data = testing_data, type = "prob") %>% bind_cols(testing_data)
# then, get the confusion matrix
confusion_matrix <- conf_mat(rfpred, yv, .pred_class)
# then, get the performance stats
performance_stats <- metrics(rfpred, yv, .pred_class)
#######################################################

  return(performance_stats)
}