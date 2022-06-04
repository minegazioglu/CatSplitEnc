library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)
library(AppliedPredictiveModeling)

 



# HPC scheduling
data(schedulingData)

dat <- data.frame(schedulingData)



# Drop duplicates 
dat <- dat %>% distinct()



# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "eucalyptus",
  description = paste("jobScheduling: HPC Job Scheduling Data
In AppliedPredictiveModeling: Functions and Data Sets for 'Applied Predictive Modeling'"),
  default.target.attribute = "Class" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://rdrr.io/rforge/AppliedPredictiveModeling/man/jobScheduling.html"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Class")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43925

#deleteOMLObject(43925, object = c("data"), verbosity = NULL)

