# To do list
# Did we really needed to do this analysis
# Analysis results here : https://www.overleaf.com/project/629f533af968deac14f7db3c


# Packages
library(dplyr)
require(data.table)
require(rpart)
require(skimr)
library(stringr)
library(janitor)
library("OpenML")
library("plyr")


binary_classification_datasets = list(43898, 43900, 43901, 43920, 43902, 43922, 43906, 41442)
multiclass_classification_datasets = list(43921, 43938, 188, 41212)
regression_datasets = list(43939, 41444, 43926, 43927)

all_datasets = c(binary_classification_datasets, multiclass_classification_datasets, regression_datasets)



res <- data.frame()

for (id in all_datasets){
# Dataset Information
datInfo <- getOMLDataSet(data.id = id, verbosity = 0)
targetVariable <- datInfo$target.features

# Clean targetVariable name beforehand as we will clean it eventually
#targetVariable <- janitor::make_clean_names(targetVariable)
dat <- datInfo$data
dat <- Filter(function(x) length(unique(na.omit(unlist(x)))) > 1, dat)
datasetName <- datInfo$desc$name
print(datasetName)
# Find categorical variables
catVariables <- names(Filter(is.factor, dat))

# Dataset Information
#if(dim(dat)[1] <= 50000){
number_of_rows = dim(dat)[1]
#}else{number_of_rows = 50000}

number_of_columns = dim(dat)[2]
number_of_numerical_columns =  length(Filter(is.numeric, dat))
number_of_categorical_columns = length(Filter(is.factor, dat))

if (id %in% c(binary_classification_datasets, multiclass_classification_datasets)){
targetVariableUniqueValueCount = length(unique(dat[[targetVariable]]))
} else {targetVariableUniqueValueCount <- 0}


categorical_column_cardinalities = t(data.frame(sapply(dat[catVariables], n_distinct, na.rm = T)))
names(categorical_column_cardinalities) = datasetName

categorical_column_max_cardinality = max(categorical_column_cardinalities)
categorical_column_mean_cardinality = mean(categorical_column_cardinalities)
categorical_column_sum_cardinality = sum(categorical_column_cardinalities)
categorical_column_percent_cardinality =  (max(categorical_column_cardinalities)/number_of_rows)*100

dat_info = data.frame(number_of_categorical_columns, categorical_column_max_cardinality, categorical_column_mean_cardinality, categorical_column_sum_cardinality, categorical_column_percent_cardinality)

#dat_info = cbind(dat_info, categorical_column_cardinalities)

rownames(dat_info) = datasetName


res <- rbind(res, dat_info)
}



write.csv(res, "categorical_properties.csv", row.names = TRUE)


# Finding distance matrix
distance_mat <- dist(scale(res), method = 'euclidean')
#distance_mat
par(mfrow=c(2,2))
#par(fig = c(4,4,4,4)) 
# Fitting Hierarchical clustering Model
# to training dataset
set.seed(240)  # Setting seed
# ward.D
Hierar_cl_ward <- hclust(distance_mat, method = "ward.D")
# single 
Hierar_cl_single <- hclust(distance_mat, method = "single")
# complete
Hierar_cl_complete <- hclust(distance_mat, method = "complete")
# average
Hierar_cl_average <- hclust(distance_mat, method = "average")



# Plotting dendrogram
plot(Hierar_cl_ward)
plot(Hierar_cl_single)
plot(Hierar_cl_complete)
plot(Hierar_cl_average)

 
# Choosing no. of clusters
# Cutting tree by height
#abline(h = 110, col = "green")
 
#clusterCut <- cutree(Hierar_cl, 4)


# Plotting dendrogram
plot(Hierar_cl_ward)

clusterCut <- cutree(Hierar_cl, 6)

a = data.frame(clusterCut)

b = merge(a[order(a$clusterCut),,drop=FALSE], res, by.x = 0, by.y = 0)

write_csv(b[order(b$clusterCut),,drop=FALSE], "6_clusters.csv")

