# import NCDBRecode.R as a module
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")
source("/Users/collindougherty/Documents/Work/pipeline/backend/dtypes.r")

# read in the data
# puf_data <- read.csv("/Users/collindougherty/Downloads/Desmoplastic Melanoma.csv")
puf_data <- read.csv("/Users/collindougherty/Downloads/ncdb_santamaria.csv")

# recode the data
recoded_data <- ncdb_recode(puf_data)

# get the dtypes
dtypes <- dtype(recoded_data)

# write to csv
write.csv(dtypes, "/Users/collindougherty/Documents/Work/pipeline/tests/test_full_recode_breast.csv", row.names = FALSE)