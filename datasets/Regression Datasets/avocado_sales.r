library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)
library(lubridate,  warn.conflicts = FALSE)

# employee_salaries dataset
dat <- fread("C:/Users/SORU/Downloads/avocado.csv", stringsAsFactors = TRUE)


# Drop duplicates 
dat <- dat %>% distinct()

# Convert date column to datetime format and split it into day and  month columns (year column already exists)
dat$Date <- as.Date(dat$Date,format="%m/%d/%Y") 
dat$month <- lubridate::month(ymd(dat$Date)) 
dat$day <- lubridate::day(ymd(dat$Date))

# drop Date column
columns_to_drop <- c("Date")
dat[,columns_to_drop] <- NULL


# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "avocado_sales",
  description = paste("Historical data on avocado prices and sales volume in multiple US markets. For this version Date column is dropped and month and day information in kept."),
  default.target.attribute = "AveragePrice" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://www.kaggle.com/datasets/neuromusic/avocado-prices"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "AveragePrice")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43927

#deleteOMLObject(43927, object = c("data"), verbosity = NULL)

