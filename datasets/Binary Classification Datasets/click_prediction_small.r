library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)




# Click Prediction Small
datInfo <- getOMLDataSet(data.id = 1220, verbosity = 0)
targetVariable <- datInfo$target.features

# Clean targetVariable name beforehand as we will clean it eventually
targetVariable <- janitor::make_clean_names(targetVariable)
dat <- datInfo$data
dat <- Filter(function(x) length(unique(na.omit(unlist(x)))) > 1, dat)
datasetName <- datInfo$desc$name
dataSetDescription <- datInfo$desc$description

# Clean column names
dat <- janitor::clean_names(dat)

dat <- data.table(dat)

# Drop duplicates 
dat <- dat[!duplicated(dat)]

# Drop column which have too many unique values (as contains unique values %75 of the length of the data)
columns_to_drop <- c("query_id", "title_id", "user_id")
dat[,columns_to_drop] <- NULL

# Convert all columns to type factor
convert_to_factor = c("ad_id", "advertiser_id", "keyword_id", "description_id")
dat[, c(convert_to_factor) := lapply(.SD, factor), .SDcols = convert_to_factor]

# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "click_prediction_small",
  description = paste("Modified version of subsampled dataset from Tencent Inc. on OpenML. Duplicate rows are dropped. Columns with a high ratio of unique values are dropped. Some columns are cast to factor."),
  default.target.attribute = targetVariable,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://www.openml.org/search?type=data&sort=runs&id=1220&status=active"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = targetVariable)

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43901
 
#deleteOMLObject(43901, object = c("data"), verbosity = NULL)

