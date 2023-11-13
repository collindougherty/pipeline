library(tidyverse)

dtype <- function(df) {
  # if variable type chr, then change to factor for all vars in df
  df <- df %>% mutate_if(is.character, as.factor)
  # lets remove all dtype logi columns from df (these are most likely columns not in data dictionary)
  df <- df %>% select_if(function(x) !is.logical(x))
  return(df)
}