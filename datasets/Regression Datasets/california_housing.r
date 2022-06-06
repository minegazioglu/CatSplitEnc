library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)



# California Housing dataset

dat <- fread("C:/Users/SORU/Downloads/housing.csv", stringsAsFactors = TRUE)



# Drop duplicates 
dat <- dat %>% distinct()



# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "california_housing",
  description = paste("Median house prices for California districts derived from the 1990 census."),
  default.target.attribute = "median_house_value" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://www.kaggle.com/datasets/camnugent/california-housing-prices"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "median_house_value")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)


# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43939

#deleteOMLObject(43939, object = c("data"), verbosity = NULL)

