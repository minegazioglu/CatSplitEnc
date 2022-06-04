library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)



# kdd appetency train set
dat <- fread("C:/Users/SORU/Downloads/orange_small_train.csv", stringsAsFactors = TRUE)

labels <- fread("C:/Users/SORU/Downloads/orange_small_train_appetency.csv")

dat[,"class"] <- labels


# Drop duplicates 
dat <- dat[!duplicated(dat)]

# Cast Target column to type factor
dat$class <- as.factor(dat$class)




 
# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "kddcup2009_appetency",
  description = paste("Small version of KDD 2009 Cup data with appetency label.predict the propensity of customers to buy new products or services (appetency)"),
  default.target.attribute = "class",
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://www.kdd.org/kdd-cup/view/kdd-cup-2009/Data"
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
# Data set successfully uploaded. Data set ID: 43908

#deleteOMLObject(43908, object = c("data"), verbosity = NULL)
