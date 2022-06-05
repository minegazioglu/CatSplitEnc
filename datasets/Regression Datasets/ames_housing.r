library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)
library(AmesHousing)

# ames_housing dataset
dat = as.data.frame(make_ames())


# Drop duplicates 
dat <- dat %>% distinct()



# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "ames_housing",
  description = paste("Predict sales prices of houses. The Ames Housing dataset was compiled by Dean De Cock for use in data science education."),
  default.target.attribute = "Sale_Price" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://cran.r-project.org/web/packages/AmesHousing/AmesHousing.pdf"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Sale_Price")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43926

#deleteOMLObject(43926, object = c("data"), verbosity = NULL)

