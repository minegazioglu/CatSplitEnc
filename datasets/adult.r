
colNames <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status",
              "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss",  
               "hours_per_week", "native_country", "class")

# Adult train and test sets
train <- fread("C:/Users/SORU/Downloads/adult.csv", stringsAsFactors = TRUE)
test <- fread("C:/Users/SORU/Downloads/adult_test.csv", stringsAsFactors = TRUE)

# Assign column names
names(train) <- colNames
names(test) <- colNames

dat2 <- rbind(train, test)

# Replace question mark with nan
dat2[dat2 == '?'] <- NA

# Replace dot in class column with empty string
dat2$class <- sapply(dat2$class, function(x) gsub('\\.', "", x))

# Drop duplicates 
dat2 <- dat2[!duplicated(dat2)]

# API Key
setOMLConfig(server = NULL, verbosity = NULL, apikey = "3253de969b5b79a5c673181e7c0751cc",
             cachedir = NULL, arff.reader = NULL, confirm.upload = NULL)

# Dataset Description
new_desc = makeOMLDataSetDescription(
  id = 150635,
  name = "adult",
  description = paste("Predict whether income exceeds $50K/yr based on census data. Also known as Census Income dataset. Train and test sets combined. Null values represented with question mark is replaced with na. 52 duplicate values found and dropped"),
  default.target.attribute = "class",
  licence = "public",
  visibility = "Only me"
)

# Create Dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat2,
  colnames.old = colnames(dat2),
  colnames.new = colnames(dat2),
  target.features = "class")

# Upload dataset to openML
uploadOMLDataSet(new_oml_dat, verbosity = 2)

#deleteOMLObject(43879, object = c("data"), verbosity = NULL)
