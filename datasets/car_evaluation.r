library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)



# car evaluation data set

dat <- fread("C:/Users/SORU/Downloads/car_data.csv", stringsAsFactors = TRUE)



# Drop duplicates 
dat <- dat %>% distinct()



# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "car_evaluation",
  description = paste("Car Evaluation Database was derived from a simple hierarchical decision model originally developed for the demonstration of DEX, M. Bohanec, V. Rajkovic: Expert system for decision making. Sistemica 1(1), pp. 145-157, 1990.). In this version duplicate rows are dropped"),
  default.target.attribute = "class" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://archive.ics.uci.edu/ml/datasets/car+evaluation"
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
# Data set successfully uploaded. Data set ID: 43921

#deleteOMLObject(43921, object = c("data"), verbosity = NULL)
