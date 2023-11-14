# read in the csv file
puf_data <- read.csv("/Users/collindougherty/Downloads/Desmoplastic Melanoma.csv")

# get the source R files
source("/Users/collindougherty/Documents/Work/pipeline/backend/dtypes.r")
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")

# recode the data
recoded_data <- ncdb_recode(puf_data)
dtypes_data <- dtype(recoded_data)

head_dtypes_data <- dtypes_data[1:10,]

# write to csv
write.csv(head_dtypes_data, "/Users/collindougherty/Documents/Work/pipeline/tests/test_head_recode.csv", row.names = FALSE)