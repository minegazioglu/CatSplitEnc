library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)



# kdd_internet_usage data set
# Dataset Information
# saveOMLConfig(arff.reader = "RWeka", overwrite = TRUE)
datInfo <- getOMLDataSet(data.id = 4133, verbosity = 0)
targetVariable <- datInfo$target.features

# Clean targetVariable name beforehand as we will clean it eventually
targetVariable <- janitor::make_clean_names(targetVariable)
dat <- datInfo$data
dat <- Filter(function(x) length(unique(na.omit(unlist(x)))) > 1, dat)
datasetName <- datInfo$desc$name
dataSetDescription <- datInfo$desc$description

# Clean column names
dat <- janitor::clean_names(dat)






# Drop duplicates 
dat <- dat %>% distinct()

# Cast Target column to type factor
#dat[targetVariable] <- as.factor(dat[targetVariable])

# Drop who column

dat <- dat[ , !(names(dat) %in% c("who"))]


# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "kdd_internet_usage",
  description = paste("This data contains general demographic information on internet users in 1997."),
  default.target.attribute = "who_pays_for_access_work" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://kdd.ics.uci.edu/databases/internet_usage/internet_usage.html"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "who_pays_for_access_work")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43920

#deleteOMLObject(43920, object = c("data"), verbosity = NULL)
