library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)



# nursery dataset

dat <- fread("C:/Users/SORU/Downloads/nursery.csv", stringsAsFactors = TRUE)



# Drop duplicates 
dat <- dat %>% distinct()



# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "mushroom",
  description = paste("Nursery Database was derived from a hierarchical decision model originally developed to rank applications for nursery schools."),
  default.target.attribute = "class" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://archive.ics.uci.edu/ml/datasets/nursery"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "class")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43923

#deleteOMLObject(43923, object = c("data"), verbosity = NULL)

