library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)



# nursery dataset

dat <- fread("C:/Users/SORU/Downloads/dataset_194_eucalyptus.csv", stringsAsFactors = TRUE)



# Drop duplicates 
dat <- dat %>% distinct()



# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "eucalyptus",
  description = paste("Find out which seedlots in a species are best for soil conservation in dry hill country. Bulluch B. T., (1992) Eucalyptus Species Selection for Soil Conservation in Seasonally Dry Hill Country - Twelfth Year Assessment New Zealand Journal of Forestry Science 21(1): 10 - 31 (1991)

Kirsten Thomson and Robert J. McQueen (1996) Machine Learning Applied to Fourteen Agricultural Datasets. University of Waikato Research Report
https://www.cs.waikato.ac.nz/ml/publications/1996/Thomson-McQueen-96.pdf + the original publication:"),
  default.target.attribute = "Utility" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://www.kaggle.com/datasets/ishadss/eucalyptus"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Utility")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43924

#deleteOMLObject(43924, object = c("data"), verbosity = NULL)

