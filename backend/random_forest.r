library(tidyverse)
library(tidymodels)
library(vcd)
set.seed(1234)

# test_recode_head.csv should be ready to be trained on
# lets write a function that will take in a csv file and return a trained model

puf_data <- read.csv("/Users/collindougherty/Downloads/Desmoplastic Melanoma.csv")
# get the source R files
source("/Users/collindougherty/Documents/Work/pipeline/backend/dtypes.r")
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")

# recode the data
recoded_data <- ncdb_recode(puf_data)
dtypes_data <- dtype(recoded_data)
df <- dtypes_data

# remove every column which is more than 25% NA
df <- df[, colMeans(is.na(df)) < 0.25]

#select all vars except for MARGINS_RECODE
y_var <- "MARGINS_RECODE"
x_vars <- names(df)[!names(df) %in% c(y_var)]
# select x_vars and y_var from df
df <- df %>% select(all_of(c(x_vars, y_var)))

df <- df %>% drop_na()

data_split <- initial_split(df, prop = .8)
training_data <- training(data_split)
testing_data <- testing(data_split)

# Creating a formula string
formula_str <- paste(y_var, "~", paste(x_vars, collapse = "+"))

model_recipe <-
  recipe(as.formula(formula_str), data = training_data) %>%
  update_role(PUF_CASE_ID, new_role = "id") %>%
  # Apply step_dummy() for factor variables
  # step_dummy(all_nominal_predictors()) %>%
  # remove all zero variance predictors
  step_zv(all_predictors()) %>%
  # Normalize only numeric predictors
  step_normalize(all_numeric_predictors())

summary(model_recipe)


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

rfpredict <- rf_fit %>% predict(new_data = training_data) %>% bind_cols(training_data)
rfpredict <- rf_fit %>% predict(new_data = training_data, type= "prob") %>% bind_cols(rfpredict)
metrics(rfpredict, y_var, .pred_class)

rftestpredict <- rf_fit %>% predict(new_data = testing_data) %>% bind_cols(testing_data)
rftestpredict <- rf_fit %>% predict(new_data = testing_data, type= "prob") %>% bind_cols(rftestpredict)
print(metrics(rftestpredict, y_var, .pred_class))
tb <- metrics(rftestpredict, y_var, .pred_class)
accuracy <- tb[1, 3]
# lets run a random forest model
rf_model <-
  rand_forest(trees = 10) %>%
  set_engine("ranger", importance = 'impurity') %>%
  set_mode("classification")

rf_workflow <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(model_recipe)

rf_fit <- rf_workflow %>% fit(data = training_data)

if (accuracy == 1) {
  rf_model <- workflows::extract_fit_engine(rf_fit)
  importance <- vip::vip(rf_model)
  # importance is a plot, not a dataframe
  # so we need to extract the dataframe from the plot
  importance_df <- importance$data
  # sort the dataframe by importance
  importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
  # get the 10 most important variables along with their importance values
  most_important_vars <- importance_df[1:10, c("Variable", "Importance")]
  message("The model's accuracy is 1, which almost certainly indicates selection of variables with perfect correlation to what you are trying to predict. Variables with extremely high importance relative to the rest are likely culprits. Consider examining variables with outlier high importance values:")
  print(most_important_vars)
  # also show the plot
  #print(importance)
  # update the plot to have text explaining what the user should do
  # this is a hacky way to do it, but it works
  # first, get the plot
  plot <- importance$plot
  # then, add the text
  plot <- plot + ggplot2::annotate("text", x = 0, y = 0, label = "The model's accuracy is 1, which almost certainly indicates selection of variables with perfect correlation to what you are trying to predict. Variables with extremely high importance relative to the rest are likely culprits. Consider examining variables with outlier high importance values:", hjust = 0, vjust = 0, size = 3)
  # then, update the plot
  importance$plot <- plot
  # then, print the plot
  print(importance)
}