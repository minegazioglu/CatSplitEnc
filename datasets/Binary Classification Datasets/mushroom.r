library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)



# mushroom dataset

dat <- fread("C:/Users/SORU/Downloads/mushroom_data.csv", stringsAsFactors = TRUE)



# Drop duplicates 
dat <- dat %>% distinct()



# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "mushroom",
  description = paste("Mushroom records drawn from The Audubon Society Field Guide to North American Mushrooms (1981). G. H. Lincoff (Pres.), New York: Alfred A. Knopf"),
  default.target.attribute = "class" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://archive.ics.uci.edu/ml/datasets/mushroom"
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
# Data set successfully uploaded. Data set ID: 43922

#deleteOMLObject(43922, object = c("data"), verbosity = NULL)
