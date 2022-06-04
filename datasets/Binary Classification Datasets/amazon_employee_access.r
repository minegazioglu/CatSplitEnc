library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)

# No missing values
# All features are categorical

# Amazon employee access train set
dat <- fread("C:/Users/SORU/Downloads/amazon_employee_access_train.csv", stringsAsFactors = TRUE)


# Drop duplicates 
dat <- dat[!duplicated(dat)]

# Convert all columns to type factor
dat <- dat %>% dplyr::mutate_if(is.numeric, as.factor)

# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "amazon_employee_access",
  description = paste("Amazon Employee Access (Kaggle Competition). The data consists of real historical data collected from 2010 & 2011.  Employees are manually allowed or denied access to resources over time. You must create an algorithm capable of learning from this historical data to predict approval/denial for an unseen set of employees. "),
  default.target.attribute = "ACTION",
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://www.kaggle.com/competitions/amazon-employee-access-challenge"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "ACTION")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43900
 
#deleteOMLObject(43900, object = c("data"), verbosity = NULL)
