library("OpenML")
library("data.table")
library(dplyr,  warn.conflicts = FALSE)

# employee_salaries dataset
dat <- fread("C:/Users/SORU/Downloads/MUP_IHP_RY21_P02_V10_DY19_PrvSvc_0.csv", stringsAsFactors = TRUE)


# Drop duplicates 
dat <- dat %>% distinct()

# drop Date column
columns_to_drop <- c("Avg_Tot_Pymt_Amt", "Avg_Submtd_Cvrd_Chrg")
dat[,columns_to_drop] <- NULL


# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  
  name = "medical_charges",
  description = paste("The Medicare Inpatient Hospitals by Provider and Service dataset provides information on inpatient discharges for Original Medicare Part A beneficiaries by IPPS hospitals. It includes information on the use, payment, and hospital charges for more than 3,000 U.S. hospitals that received IPPS payments. The data are organized by hospital and Medicare Severity Diagnosis Related Group (DRG). The DRGs included in this dataset represent more than seven million discharges or 75% of total Medicare IPPS discharges.Data from 2019 is used. Avg_Tot_Pymt_Amt and Avg_Submtd_Cvrd_Chrg columns are dropped "),
  default.target.attribute = "Avg_Mdcr_Pymt_Amt" ,
  licence = "public",
  visibility = "Only me",
  original.data.url = "https://data.cms.gov/provider-summary-by-type-of-service/medicare-inpatient-hospitals/medicare-inpatient-hospitals-by-provider-and-service"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Avg_Mdcr_Pymt_Amt")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 43928

#deleteOMLObject(43928, object = c("data"), verbosity = NULL)

