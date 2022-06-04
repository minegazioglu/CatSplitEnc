library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)
library(lubridate,  warn.conflicts = FALSE)

# No missing values
# All features are categorical

# Amazon employee access train set
dat <- fread("C:/Users/SORU/Downloads/kick_train_set.csv", stringsAsFactors = TRUE, na.strings = "NULL")


# Drop duplicates 
dat <- dat[!duplicated(dat)]

# Convert all columns to type factor
dat$IsBadBuy <- as.factor(dat$IsBadBuy)

# Drop columns with null ratio greater than %95 and id columns
columns_to_drop <- c("PRIMEUNIT", "AUCGUART", "RefId")
dat[,columns_to_drop] <- NULL

# Convert date column to datetime format and split it into day, month and year columns
dat$PurchDate <- as.Date(dat$PurchDate,format="%m/%d/%Y") 

head(dat, 2)


dat$year <- lubridate::year(ymd(dat$PurchDate))
dat$month <- lubridate::month(ymd(dat$PurchDate)) 
dat$day <- lubridate::day(ymd(dat$PurchDate))

# drop year column
columns_to_drop <- c("PurchDate")
dat[,columns_to_drop] <- NULL



 
# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "kick",
  description = paste("Don't Get Kicked! dataset from Kaggle. Some modification are made: processing time column, dropping columns with high null ratio"),
  default.target.attribute = "IsBadBuy",
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://www.kaggle.com/c/DontGetKicked"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "IsBadBuy")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43902

#deleteOMLObject(43902, object = c("data"), verbosity = NULL)
