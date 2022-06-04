library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)



# Titanic train set
dat <- fread("C:/Users/SORU/Downloads/titanic_train.csv", stringsAsFactors = TRUE, na.strings = "NULL")


# Drop duplicates 
dat <- dat[!duplicated(dat)]

# Cast Target column to type factor
dat$Survived <- as.factor(dat$Survived)


# Drop id columns
columns_to_drop <- c("PassengerId")
dat[,columns_to_drop] <- NULL




 
# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "titanic",
  description = paste("Titanic competititon from Kaggle. Predict survival on the Titanic. PassengerId column is dropped"),
  default.target.attribute = "Survived",
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://www.kaggle.com/competitions/titanic/overview"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Survived")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43906

#deleteOMLObject(43906, object = c("data"), verbosity = NULL)
