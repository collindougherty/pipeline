# import NCDBRecode.R as a module
source("/Users/collindougherty/Documents/Work/pipeline/NCDBRecode.R")

# read in the data
puf_data <- read.csv("/Users/collindougherty/Downloads/Desmoplastic Melanoma.csv")

# recode the data
recoded_data <- NCDBRecode(puf_data)

print(head(recoded_data))