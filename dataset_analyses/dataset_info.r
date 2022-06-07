## To Do List:

# We can add class distributions of classification datasets
# Should we add column unique level counts as a column
# In table caption describe columns names
# Change dataset names to full names instead of shortened nicknames
# The resulting table is shared here : https://www.overleaf.com/project/629f533af968deac14f7db3c




# Packages
library(dplyr)
require(data.table)
require(rpart)
require(skimr)
library(stringr)
library(janitor)
library("OpenML")
library("plyr")

# Dataset OpenML IDs
binary_classification_datasets = list(43898, 43900, 43901, 43920, 43902, 43922, 43906, 41442)
multiclass_classification_datasets = list(43921, 43938, 188, 41212)
regression_datasets = list(43939, 41444, 43926, 43927)

all_datasets = c(binary_classification_datasets, multiclass_classification_datasets, regression_datasets)


res <- data.frame()

for (id in all_datasets){
# Fetch dataset from OpenML
datInfo <- getOMLDataSet(data.id = id, verbosity = 0)
targetVariable <- datInfo$target.features
dat <- datInfo$data
dat <- Filter(function(x) length(unique(na.omit(unlist(x)))) > 1, dat)
datasetName <- datInfo$desc$name

# Find categorical variables
catVariables <- names(Filter(is.factor, dat))

# Dataset Information
number_of_rows = dim(dat)[1]
number_of_columns = dim(dat)[2]
number_of_numerical_columns =  length(Filter(is.numeric, dat))
number_of_categorical_columns = length(Filter(is.factor, dat))

if (id %in% c(binary_classification_datasets, multiclass_classification_datasets)){
targetVariableUniqueValueCount = length(unique(dat[[targetVariable]]))
} else {targetVariableUniqueValueCount <- 0}


if (id %in% c(binary_classification_datasets))
{tasktype = "Binary Classification"} 
else if (id %in% c(multiclass_classification_datasets))
{tasktype = "Multiclass Classification"}
else {tasktype = "Regression"}



categorical_column_cardinalities = t(data.frame(sapply(dat[catVariables], n_distinct, na.rm = T)))
names(categorical_column_cardinalities) = datasetName

categorical_column_max_cardinality = max(categorical_column_cardinalities)
categorical_column_mean_cardinality = mean(categorical_column_cardinalities)
categorical_column_sum_cardinality = sum(categorical_column_cardinalities)
categorical_column_percent_cardinality =  (max(categorical_column_cardinalities)/number_of_rows)*100

dat_info = data.frame(id, datasetName, number_of_rows, tasktype, targetVariableUniqueValueCount,  number_of_categorical_columns, number_of_numerical_columns, categorical_column_max_cardinality, round(categorical_column_mean_cardinality, 2), categorical_column_sum_cardinality)

#dat_info = cbind(dat_info, categorical_column_cardinalities)

#rownames(dat_info) = datasetName


res <- rbind(res, dat_info)
}

colnames(res) <- c("OpenML_ID", "Name", "N", "TaskType", "N_Class", "K", "P", "max_cardinality", "mean_cardinality", "sum_cardinality"  )

write.csv(res, "data_info.csv", row.names = FALSE)
