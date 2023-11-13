# import NCDBRecode.R as a module
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")

# read in the data
puf_data <- read.csv("/Users/collindougherty/Downloads/Desmoplastic Melanoma.csv")

# recode the data
recoded_data <- ncdb_recode(puf_data)

# write to csv
write.csv(recoded_data, "/Users/collindougherty/Documents/Work/pipeline/tests/test_full_recode.csv", row.names = FALSE)