# import NCDBRecode.R as a module
source("/Users/collindougherty/Documents/Work/pipeline/NCDBRecode.R")

# read in the data
puf_data <- read.csv("/Users/collindougherty/Downloads/Desmoplastic Melanoma.csv")

# recode the data
recoded_data <- ncdb_recode(puf_data)

recoded_data_head <- head(recoded_data)

# write to csv
write.csv(recoded_data_head, "/Users/collindougherty/Documents/Work/pipeline/test_recode.csv", row.names = FALSE)