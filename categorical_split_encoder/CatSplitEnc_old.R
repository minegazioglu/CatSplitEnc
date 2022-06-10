catSplitEncoding <- function(targetVariable = targetVariable, trainData = train, testData = test, problemType = "classification", datasetName = datasetName, catVariables = catVariables){
# Impute train and test sets if any NAa exist
#imp = impute(train, target = targetVariable, classes = list(numeric = imputeLearner("regr.rpart"), integer = imputeLearner("regr.rpart"), factor = imputeLearner("classif.rpart")))
#train = imp$data
#test = reimpute(test, imp$desc)
formula <- paste(targetVariable, "~.", collapse = "")
datasetNameTxt <- paste0(datasetName, ".txt", collapse = "")
catVariablesWithSpaces <- list()
for(cat in catVariables){
  catVariablesWithSpaces <- append(catVariablesWithSpaces, paste0(cat, " ",""))
}
if(problemType == "regression"){
# Fit rpart
tree_fit = rpart(formula, train, method = 'anova', control = rpart.control(cp=0,maxdepth=5, usesurrogate = 2))
} else if(problemType == "classification"){
tree_fit = rpart(formula, train, method = 'class', control = rpart.control(cp=0,maxdepth=5,usesurrogate = 2))
}

# Get the summary file of tree_fit
summary(tree_fit, file = datasetNameTxt)

# Get a list of ordered unique names from rpart for all categorical variables
# Resulting list is a named list, names being categorical variable names
uniqueSortedNames <- list()

for(i in 1:length(names(attr(tree_fit,"xlevels")))){
  names <- names(attr(tree_fit,"xlevels"))
  uniqueSortedNames[[names[i]]] <- data.table(lev = attr(tree_fit,"xlevels")[[i]])
}

# Read summary file
allLines <- readLines(datasetNameTxt)

# Filter allLines and remove the unnecessary lines
# unnecessary lines being the node numbers which do not have primary or surrogate split information
# or predicted class, variable importance etc.

necessaryLines <- c()


necessaryLinescheckList <- unlist(list("Node number", "Primary splits", "Surrogate splits", catVariablesWithSpaces))

for(i in 1:length(allLines)){
  if(grepl(paste(necessaryLinescheckList, collapse="|"), allLines[i])){
    necessaryLines <- append(necessaryLines, trimws(allLines[i]))
  }
}

necessaryLines[1] <- NA

necessaryLinesCopy <- necessaryLines
indicestoRemove <- list()

for(i in 1:(length(necessaryLinesCopy)-1)){
  if(grepl("Node number", necessaryLinesCopy[[i]]) && grepl("Node number", necessaryLinesCopy[[i + 1]]) ){
    indicestoRemove <- append(indicestoRemove, i)
    
  }
}

for(i in indicestoRemove){
  necessaryLines[[i]] <- NA
}

necessaryLines <- necessaryLines[!is.na(necessaryLines)]

if(grepl("Node number", necessaryLines[length(necessaryLines)])){
  necessaryLines[length(necessaryLines)] <- NA
}
# necessaryLines consists of node numbers, primary and surrogate split information
necessaryLines <- necessaryLines[!is.na(necessaryLines)]

#-----------------------------------------------------------------------------------------------

# SPLIT necessarylines into Primary and Surrogate Splits

# PRIMARY SPLITS
primaryfullNames <- stack(Filter(Negate(is.null),
             lapply(split(necessaryLines, cumsum(grepl('Node number', necessaryLines))), 
                    function(x) {
                      x1 <- sub(",.*", "", x[grep('improve', x)])
                      if(length(x1) > 0) paste(x1, strsplit(x[1],":")[[1]][1])
                    })))[2:1][[2]]

primaryfullNamesModified <- list()

for(name in primaryfullNames){
  primaryfullNamesModified <- append(primaryfullNamesModified, paste(c(strsplit(name, "\\s+")[[1]][c(1,5,6,7)],"primary"), collapse = " "))
}


primaryInfo <- list()
primarygeneralName <- list()
primaryPattern <- "splits as\\s*(.*?)\\s,*improve"

for(i in necessaryLines){
  if(grepl("improve",i)){
    #print(i)
    name <- strsplit(i, "\\s+")[[1]][1]
    #print(name)
    primarygeneralName <- append(primarygeneralName, name)
    primaryInfo <- append(primaryInfo, trimws(sub(",","",regmatches(i, regexec(primaryPattern, i))[[1]][2])))
  }
}

primaryfullNamesModified <- unlist(primaryfullNamesModified)

primaryInfo <- unlist(primaryInfo)

names(primaryInfo) <- unlist(primaryfullNamesModified)


primaryInfo <- lapply(split(x = primaryInfo, f = sapply(strsplit(names(primaryInfo)," ") , "[", 1)), unlist)


# SURROGATE SPLITS
surrogatefullNames <- stack(Filter(Negate(is.null),
                                 lapply(split(necessaryLines, cumsum(grepl('Node number', necessaryLines))), 
                                        function(x) {
                                          x1 <- sub(",.*", "", x[grep('agree', x)])
                                          if(length(x1) > 0) paste(x1, strsplit(x[1],":")[[1]][1])
                                        })))[2:1][[2]]

surrogatefullNamesModified <- list()

for(name in surrogatefullNames){
  surrogatefullNamesModified <- append(surrogatefullNamesModified, paste(c(strsplit(name, "\\s+")[[1]][c(1,5,6,7)], "surrogate"), collapse = " "))
}


surrogateInfo <- list()
surrogategeneralName <- list()
surrogatePattern <- "splits as\\s*(.*?)\\s,*agree"

for(i in necessaryLines){
  if(grepl("agree",i)){
    name <- strsplit(i, "\\s+")[[1]][1]
    surrogategeneralName <- append(surrogategeneralName, name)
    surrogateInfo <- append(surrogateInfo, trimws(sub(",","",regmatches(i, regexec(surrogatePattern, i))[[1]][2])))
  }
}

surrogatefullNamesModified <- unlist(surrogatefullNamesModified)

surrogateInfo <- unlist(surrogateInfo)

names(surrogateInfo) <- unlist(surrogatefullNamesModified)


surrogateInfo <- lapply(split(x = surrogateInfo, f = sapply(strsplit(names(surrogateInfo)," ") , "[", 1)), unlist)
#print(surrogateInfo)
#---------------------------------------------------------------------------

# Convert Primary and Surrogate Splits into Separate Dataframes

## PRIMARY

primaryDF <- c()
for(i in 1:length(names(primaryInfo))){
  subDf <- c()
  for(j in 1:length(primaryInfo[[i]])) {
  subDf[[j]] <- data.table(strsplit(unname(trimws(primaryInfo[[i]])),"")[[j]])
  }
  subDf <- data.frame(subDf)
  setnames(subDf, names(primaryInfo[[i]]))
  primaryDF[[names(primaryInfo)[[i]]]] <- subDf
}

# Bring unique sorted names next to each categorical column name

primaryDfModified <- c()

for(i in names(uniqueSortedNames)){
  for(j in names(primaryDF)){
    if(i == j){
      primaryDfModified[[i]] <- cbind(primaryDF[[j]], uniqueSortedNames[[i]])
    }
  }
}

# Map the columns to the dataframe

primaryDFNames <- names(primaryDfModified)
# PrimaryOut is the dataframe with dat and primary columns added
primaryOut <- trainData

primaryOut$id  <- 1:nrow(primaryOut)
for(i in seq_along(primaryDFNames)) {
  primaryOut <- merge(primaryOut, primaryDfModified[[primaryDFNames[i]]], all.x = TRUE,
               by.x = primaryDFNames[i], by.y = 'lev')
}

primaryOut <- primaryOut[order(primaryOut$id), ]

primaryOut <- data.frame(primaryOut, check.names = F)

# Test Data
primaryOutTest <- testData

primaryOutTest$id  <- 1:nrow(primaryOutTest)
for(i in seq_along(primaryDFNames)) {
  primaryOutTest <- merge(primaryOutTest, primaryDfModified[[primaryDFNames[i]]], all.x = TRUE,
               by.x = primaryDFNames[i], by.y = 'lev')
}

primaryOutTest <- primaryOutTest[order(primaryOutTest$id), ]

primaryOutTest <- data.frame(primaryOutTest, check.names = F)

# Column that do not have the value "-"

primaryColumnstoBeEdited <- list()

for(i in 1:length(names(primaryDF))){
  uniqueValues <- lapply(primaryDF[[i]],unique)
  #print(uniqueValues)
  notFullsub <- c()
  for(j in 1:length(uniqueValues)){
    if("-" %in% uniqueValues[[j]]){
      notFullsub <- append(notFullsub, names(uniqueValues)[[j]]) 
    }
  }
  primaryColumnstoBeEdited[[names(primaryDF)[[i]]]] <- notFullsub
}


# For primary columns if the column is not the most important split and contains "-"
# we throw it
if(problemType == "classification"){
if(length(primaryColumnstoBeEdited) != 0){
primaryColumnstoBeEditedtoKeep <- list()
for(j in 1:length(names(primaryColumnstoBeEdited))){
for(i in primaryColumnstoBeEdited[[j]]){
    nodeName <- paste0(paste(unlist(strsplit(i," ")[[1]])[2:4], collapse = " "),":") 
    catColName <- paste(unlist(strsplit(i," ")[[1]])[1], collapse = " ")
    for(line in 1:length(allLines)){
      if(grepl(nodeName, allLines[[line]])){
        if(unlist(strsplit(trimws(allLines[[line + 6]])," ")[[1]])[1] == strsplit(i, " ")[[1]][1]){
          primaryColumnstoBeEditedtoKeep <- append(primaryColumnstoBeEditedtoKeep, i)
        }
      }
    }
  }
}
}else {primaryColumnstoBeEditedtoKeep <- list() 
       cat(paste("Primary column do not contain any non-directional information", "Therefore, no need to encode any primary columns from surrogates", sep = "\n"),  sep = "\n")}
}else if(problemType == "regression"){
  if(length(primaryColumnstoBeEdited) != 0){
primaryColumnstoBeEditedtoKeep <- list()
for(j in 1:length(names(primaryColumnstoBeEdited))){
for(i in primaryColumnstoBeEdited[[j]]){
    nodeName <- paste0(paste(unlist(strsplit(i," ")[[1]])[2:4], collapse = " "),":") 
    catColName <- paste(unlist(strsplit(i," ")[[1]])[1], collapse = " ")
    for(line in 1:length(allLines)){
      if(grepl(nodeName, allLines[[line]])){
        if(unlist(strsplit(trimws(allLines[[line + 4]])," ")[[1]])[1] == strsplit(i, " ")[[1]][1]){
          primaryColumnstoBeEditedtoKeep <- append(primaryColumnstoBeEditedtoKeep, i)
        }
      }
    }
  }
}
}else {primaryColumnstoBeEditedtoKeep <- list() 
       cat(paste("Primary column do not contain any non-directional information", "Therefore, no need to encode any primary columns from surrogates", sep = "\n"),  sep = "\n")}
}
# Remove primary columns to be discarded
# I realized I need to discard primary columns who are on the first row
# and do not have any corresponding surrogate splits should be discarded as well

primaryColumnstobeDiscarded <- setdiff( unlist(unname(primaryColumnstoBeEdited)), primaryColumnstoBeEditedtoKeep)

# Find further primary columns to be discarded that do not have any corresponding surrogate splits
primaryANDsurrogatelinesANDnodeslist <- list("Node number", "Primary splits", "Surrogate splits")
primaryANDsurrogatelinesANDnodes <- list()
for(line in 1:length(necessaryLines)){
  if(grepl(paste(primaryANDsurrogatelinesANDnodeslist, collapse="|"), necessaryLines[[line]])){
    primaryANDsurrogatelinesANDnodes <- append(primaryANDsurrogatelinesANDnodes, necessaryLines[[line]])
  }
}

primaryANDsurrogatelinesANDnodes[[length(primaryANDsurrogatelinesANDnodes) + 1 ]] <- "a"

NodeNumbersNottoBeDiscardedforPrimary <- list()
for(line in 1:length(primaryANDsurrogatelinesANDnodes)){
  if(grepl("Node number",primaryANDsurrogatelinesANDnodes[[line]] )){
    if(grepl("Primary splits",primaryANDsurrogatelinesANDnodes[[line+1]]) && grepl("Surrogate splits",primaryANDsurrogatelinesANDnodes[[line+2]]) ){
      NodeNumbersNottoBeDiscardedforPrimary <- append(NodeNumbersNottoBeDiscardedforPrimary, sub(":","",paste(strsplit(primaryANDsurrogatelinesANDnodes[[line]], " ")[[1]][1:3], collapse = " ")))
    }
  }
}

primaryColumnstobeDiscarded2 <- list()


for(column in primaryColumnstoBeEditedtoKeep){
  if(!(paste(strsplit(column, " ")[[1]][2:4], collapse = " ") %in% NodeNumbersNottoBeDiscardedforPrimary)){
    primaryColumnstobeDiscarded2 <- append(primaryColumnstobeDiscarded2, column)
  }
}
# Unite two information of primary columns to be dropped
primaryColumnstobeDiscarded <- unlist(c(list(primaryColumnstobeDiscarded), primaryColumnstobeDiscarded2))


# Drop from editedtokeeplist
primaryColumnstoBeEditedtoKeepCopy <- primaryColumnstoBeEditedtoKeep

if(length(primaryColumnstoBeEditedtoKeepCopy) != 0){

for(column in 1:length(primaryColumnstoBeEditedtoKeepCopy)){
  if(primaryColumnstoBeEditedtoKeepCopy[[column]] %in% primaryColumnstobeDiscarded2){
    primaryColumnstoBeEditedtoKeep[[column]] <- NULL
  }
}
}#else cat(paste("There are no primary columns to be dropped", "because none of them contains non-directional information", sep = "\n"), sep = "\n")


# Find the corresponding surrogate split line
# First find categorical surrogate splits

namesPrimaryCategoricalSplits <- list()
primarySplitEncodingsCategorical <- list()
lineNumberPrimaryCategorical <- list()


#catVariablesstartANDend <- list()
#namesPrimaryCategoricalSplits <- list()
#primarySplitEncodingsCategorical <- list()
#lineNumberPrimaryCategorical <- list()

catVariablesstartANDend <- list()

for(cat in catVariables){
  catVariablesstartANDend <- append(catVariablesstartANDend, paste0("^",cat,"$"))}

if(length(primaryColumnstoBeEditedtoKeep)!= 0){

catVariablesstartANDend <- list()

for(cat in catVariables){
  catVariablesstartANDend <- append(catVariablesstartANDend, paste0("^",cat,"$"))
}

namesPrimaryCategoricalSplits <- list()
primarySplitEncodingsCategorical <- list()
lineNumberPrimaryCategorical <- list()

for(i in 1:length(primaryColumnstoBeEditedtoKeep)){
  nodeNumber <- paste0(paste(unlist(strsplit(primaryColumnstoBeEditedtoKeep[[i]]," ")[[1]])[2:4], collapse = " "),":")
  counter <- 0
  counter2 <- 0
  for(l in 1:length(allLines)){
    counter <- counter + 1
    counter2 <- counter2 + 1
    if(grepl(nodeNumber,allLines[[l]])){
      for(lr in allLines[counter:length(allLines)]){
        counter2 <- counter2 + 1
        if(grepl("agree",lr)){
          if(grepl(paste(catVariablesstartANDend, collapse="|"), strsplit(trimws(lr)," ")[[1]][1] ))
          {
            a <-  paste(c(strsplit(trimws(lr)," ")[[1]][1], strsplit(primaryColumnstoBeEditedtoKeep[[i]], " ")[[1]][2:4], "surrogate"), collapse = " " )
            primarySplitEncodingsCategorical <- append(primarySplitEncodingsCategorical, a )
            namesPrimaryCategoricalSplits <- append(namesPrimaryCategoricalSplits, primaryColumnstoBeEditedtoKeep[[i]])
            lineNumberPrimaryCategorical <- append(lineNumberPrimaryCategorical, counter2 - 1)
            break
          }
        }
      }
    }
  }
}
} #else cat("There are no primary columns to be encoded", sep="\n")
# Find the encodings for numerical surrogates too

names(primarySplitEncodingsCategorical) <- namesPrimaryCategoricalSplits


# Save columns to be discarded in a df
primaryDiscardedDf <- primaryOut[primaryColumnstobeDiscarded]
#test
primaryDiscardedDfTest <- primaryOutTest[primaryColumnstobeDiscarded]

# Remove primary columns to be discarded from the dataframe
primaryOut <- primaryOut[ , !names(primaryOut) %in% c(primaryColumnstobeDiscarded)]
#test
primaryOutTest <- primaryOutTest[ , !names(primaryOutTest) %in% c(primaryColumnstobeDiscarded)]

#####------------------------------------------------------------------------------------------
#####------------------------------------------------------------------------------------------

# Convert Primary and Surrogate Splits into Separate Dataframes

## SURROGATE

surrogateDF <- c()
for(i in 1:length(names(surrogateInfo))){
  subDf <- c()
  for(j in 1:length(surrogateInfo[[i]])) {
  subDf[[j]] <- data.table(strsplit(unname(trimws(surrogateInfo[[i]])),"")[[j]])
  }
  subDf <- data.frame(subDf)
  setnames(subDf, names(surrogateInfo[[i]]))
  surrogateDF[[names(surrogateInfo)[[i]]]] <- subDf
}

# Bring unique sorted names next to each categorical column name

surrogateDfModified <- c()

for(i in names(uniqueSortedNames)){
  for(j in names(surrogateDF)){
    if(i == j){
      surrogateDfModified[[i]] <- cbind(surrogateDF[[j]], uniqueSortedNames[[i]])
    }
  }
}

# Map the columns to the dataframe
surrogateDFNames <- names(surrogateDfModified)

# surrogateOut is the dataframe with dat and surrogate columns added


surrogateOut <- trainData
surrogateOut$id  <- 1:nrow(surrogateOut)

for(i in seq_along(surrogateDFNames)) {
  surrogateOut <- merge(surrogateOut, surrogateDfModified[[surrogateDFNames[i]]], all.x = TRUE,
               by.x = surrogateDFNames[i], by.y = 'lev')
}

surrogateOut <- surrogateOut[order(surrogateOut$id), ]

surrogateOut <- data.frame(surrogateOut, check.names = F)

## For TEST

surrogateOutTest <- testData
surrogateOutTest$id  <- 1:nrow(surrogateOutTest)

for(i in seq_along(surrogateDFNames)) {
  surrogateOutTest <- merge(surrogateOutTest, surrogateDfModified[[surrogateDFNames[i]]], all.x = TRUE,
               by.x = surrogateDFNames[i], by.y = 'lev')
}

surrogateOutTest <- surrogateOutTest[order(surrogateOutTest$id), ]

surrogateOutTest <- data.frame(surrogateOutTest, check.names = F)

# Find surrogate columns to be edited which means they have "-"

surrogateColumnstoBeEdited <- list()

for(i in 1:length(names(surrogateDF))){
  uniqueValues <- lapply(surrogateDF[[i]],unique)
  notFullsub <- c()
  for(j in 1:length(uniqueValues)){
    if("-" %in% uniqueValues[[j]]){
      notFullsub <- append(notFullsub, names(uniqueValues)[[j]])
    }
  }
  surrogateColumnstoBeEdited[[names(surrogateDF)[[i]]]] <- notFullsub
}

# Get rid of names
surrogateColumnstoBeEdited <- unname(surrogateColumnstoBeEdited)
surrogateColumnstoBeEdited <- unlist(unname(surrogateColumnstoBeEdited))

##-------------------------------------------------------------------------------------------

# Find the corresponding primary columns to surrogate columns to be edited
# First for categorical surrogate splits

## We have to state the type of the problem for this part
namesSurrogateCategoricalSplits <- list()
surrogateSplitEncodingsCategorical <- list()
namesSurrogateNumericalSplits <- list()
surrogateSplitEncodingsNumerical <- list()

if(problemType == "classification"){

namesSurrogateCategoricalSplits <- list()
surrogateSplitEncodingsCategorical <- list()
if(!is.null(surrogateColumnstoBeEdited)){
namesSurrogateCategoricalSplits <- list()
surrogateSplitEncodingsCategorical <- list()
for(i in 1:length(surrogateColumnstoBeEdited)){
  node_number <- paste0(paste(strsplit(surrogateColumnstoBeEdited[[i]], " ")[[1]][2:4], collapse = " "),":")
  counter <- 0
  for(line in 1:length(allLines)){
  counter <- counter + 1
  if(grepl(node_number,allLines[[line]])){
    if( (grepl(paste(catVariablesstartANDend, collapse="|"), strsplit(trimws(allLines[[counter+6]])," ")[[1]][1] ))){
      columnName <- strsplit(trimws(allLines[[counter+6]])," ")[[1]][1]
      wholeName <- paste(c(columnName,node_number,"primary"), collapse = " ")
      surrogateSplitEncodingsCategorical <- append(surrogateSplitEncodingsCategorical, sub(":","",wholeName))
      namesSurrogateCategoricalSplits <- append(namesSurrogateCategoricalSplits,sub(":","",surrogateColumnstoBeEdited[[i]]))
      }
    }
  }
}

names(surrogateSplitEncodingsCategorical) <- namesSurrogateCategoricalSplits


} else{cat("There are no surrogate columns to be edited", sep="\n")}


# Second for numerical surrrogate splits

# Find the corresponding primary columns to surrogate columns to be edited
namesSurrogateNumericalSplits <- list()
surrogateSplitEncodingsNumerical <- list()
if(!is.null(surrogateColumnstoBeEdited)){
namesSurrogateNumericalSplits <- list()
surrogateSplitEncodingsNumerical <- list()
for(i in 1:length(surrogateColumnstoBeEdited)){
  node_number <- paste0(paste(strsplit(surrogateColumnstoBeEdited[[i]], " ")[[1]][2:4], collapse = " "),":")
  counter <- 0
  for(line in 1:length(allLines)){
  counter <- counter + 1
  if(grepl(node_number,allLines[[line]])){
    if( !(grepl(paste(catVariablesstartANDend, collapse="|"), strsplit(trimws(allLines[[counter+6]])," ")[[1]][1] ))){
      columnName <- strsplit(trimws(allLines[[counter+6]])," ")[[1]][1]
      #wholeName <- paste(c(columnName,sub(":","",node_number),"primary"), collapse = " ")
      wholeName <- columnName
      #print(wholeName)
      stringToBeManipulated <- list(strsplit(trimws(allLines[[counter+6]])," ")[[1]])
      stringToBeManipulatedCleaned <- lapply(stringToBeManipulated, function(x) x[!x %in% ""])[[1]]
      #print(stringToBeManipulatedCleaned)
      value <- paste(stringToBeManipulatedCleaned[2:3], collapse = " ")
      direction <- stringToBeManipulatedCleaned[6]
      mainInfo <- paste(c(wholeName, value , direction), collapse = " ")
      surrogateSplitEncodingsNumerical <- append(surrogateSplitEncodingsNumerical, mainInfo)
      namesSurrogateNumericalSplits <- append(namesSurrogateNumericalSplits,surrogateColumnstoBeEdited[[i]])
      }
    }
  }
}

names(surrogateSplitEncodingsNumerical) <- namesSurrogateNumericalSplits
} else{
  namesSurrogateNumericalSplits <- list()
  surrogateSplitEncodingsNumerical <- list()}

} else if(problemType == "regression"){
  namesSurrogateCategoricalSplits <- list()
surrogateSplitEncodingsCategorical <- list()
if(!is.null(surrogateColumnstoBeEdited)){
namesSurrogateCategoricalSplits <- list()
surrogateSplitEncodingsCategorical <- list()
for(i in 1:length(surrogateColumnstoBeEdited)){
  node_number <- paste0(paste(strsplit(surrogateColumnstoBeEdited[[i]], " ")[[1]][2:4], collapse = " "),":")
  counter <- 0
  for(line in 1:length(allLines)){
  counter <- counter + 1
  if(grepl(node_number,allLines[[line]])){
    if( (grepl(paste(catVariablesstartANDend, collapse="|"), strsplit(trimws(allLines[[counter+4]])," ")[[1]][1] ))){
      columnName <- strsplit(trimws(allLines[[counter+4]])," ")[[1]][1]
      wholeName <- paste(c(columnName,node_number,"primary"), collapse = " ")
      surrogateSplitEncodingsCategorical <- append(surrogateSplitEncodingsCategorical, sub(":","",wholeName))
      #print("not correct")
      namesSurrogateCategoricalSplits <- append(namesSurrogateCategoricalSplits,sub(":","",surrogateColumnstoBeEdited[[i]]))
      }
    }
  }
}

names(surrogateSplitEncodingsCategorical) <- namesSurrogateCategoricalSplits


} else{cat("There are no surrogate columns to be edited", sep="\n")}



# Second for numerical surrrogate splits

# Find the corresponding primary columns to surrogate columns to be edited
namesSurrogateNumericalSplits <- list()
surrogateSplitEncodingsNumerical <- list()
if(!is.null(surrogateColumnstoBeEdited)){
namesSurrogateNumericalSplits <- list()
surrogateSplitEncodingsNumerical <- list()
for(i in 1:length(surrogateColumnstoBeEdited)){
  node_number <- paste0(paste(strsplit(surrogateColumnstoBeEdited[[i]], " ")[[1]][2:4], collapse = " "),":")
  counter <- 0
  for(line in 1:length(allLines)){
  counter <- counter + 1
  if(grepl(node_number,allLines[[line]])){
    if( !(grepl(paste(catVariablesstartANDend, collapse="|"), strsplit(trimws(allLines[[counter+4]])," ")[[1]][1] ))){
      columnName <- strsplit(trimws(allLines[[counter+4]])," ")[[1]][1]
      #wholeName <- paste(c(columnName,sub(":","",node_number),"primary"), collapse = " ")
      wholeName <- columnName
      #print(wholeName)
      stringToBeManipulated <- list(strsplit(trimws(allLines[[counter+6]])," ")[[1]])
      stringToBeManipulatedCleaned <- lapply(stringToBeManipulated, function(x) x[!x %in% ""])[[1]]
      #print(stringToBeManipulatedCleaned)
      value <- paste(stringToBeManipulatedCleaned[2:3], collapse = " ")
      direction <- stringToBeManipulatedCleaned[6]
      mainInfo <- paste(c(wholeName, value , direction), collapse = " ")
      surrogateSplitEncodingsNumerical <- append(surrogateSplitEncodingsNumerical, mainInfo)
      #print("Correct")
      namesSurrogateNumericalSplits <- append(namesSurrogateNumericalSplits,surrogateColumnstoBeEdited[[i]])
      }
    }
  }
}

names(surrogateSplitEncodingsNumerical) <- namesSurrogateNumericalSplits
} else{
  namesSurrogateNumericalSplits <- list()
  surrogateSplitEncodingsNumerical <- list()}
}

# Unite Primary and Surrogate Dfs
DfOut <- merge(primaryOut, surrogateOut)
DfOutTest <- merge(primaryOutTest, surrogateOutTest)

# Filter out duplicate columns 
DfOut <- DfOut[, !duplicated(colnames(DfOut))]
DfOut <- DfOut[order(DfOut$id), ]
#Test
DfOutTest <- DfOutTest[, !duplicated(colnames(DfOutTest))]
DfOutTest <- DfOutTest[order(DfOutTest$id), ]
     
### ----------------------------------------------------------------------------

# ENCODING PRIMARIES
# Encode primaries from numerical and categorical splits
ColumnNamesfromColumnNumber = primaryColumnstoBeEditedtoKeep 


# Here a start index and ending index is obtained to find the surrogate elements in between

startIndex <- list()
endIndex <- list()
for(name in ColumnNamesfromColumnNumber){
  # extract node number
  nodeNumberofprimary <- paste0(paste(strsplit(name," ")[[1]][2:4], collapse = " "),":")
  for(i in 1:length(allLines)){
    if(grepl(nodeNumberofprimary, allLines[[i]])){
      startIndex <- append(startIndex, i)
      for(i2 in i+1:length(allLines)){
        if(grepl("Node number", allLines[[i2]])){
          endIndex <- append(endIndex, i2)
          break
        }
      }
    }
  }
}

names(startIndex) <- endIndex


# Get all the lines for lines between each start index and end index
lineList <- list()
if(length(startIndex) != 0){
lineList <- list()


for(l in 1:length(startIndex)){
  lineList <- append(lineList, trimws(allLines[startIndex[[l]]:names(startIndex)[[l]] -1]))
}
} #else cat("Nothing to be encoded", sep="\n")

# From line list filter all the columns that are under surrogate splits
if(length(lineList) != 0){
out <- by(unlist(lineList),
          cumsum(grepl("^(Node|Surrogate)", lineList)),
          function(x) {
            if (grepl("Node number.*:", x[1])) x[1] else {
              if (grepl("Surrogate.*:", x[1])) x[-1]
            }
          })

outList <- as.list(do.call(c, unname(out)))

outList <- lapply(outList, function(z){ z[z != ""]})

outList <- Filter(length, outList)

# Create a nested list, where each key is a "number XX" and its elements are the variables needed
indicesOfNodes = which(grepl("Node number", outList))
mappingListNested = lapply(seq_along(indicesOfNodes), function(i){
  # If part is currently added
  if (length(outList) == 1){
    return(outList[(length(outList))])
  }
  else if (i+1 <= length(indicesOfNodes)){
    return(outList[(indicesOfNodes[i]+1):(indicesOfNodes[i+1]-1)])
  } else {
    return(outList[(indicesOfNodes[i]+1):length(outList)])
  }
})
oppositeDirection <- function(direction){
  if(direction == "L"){return("R")}
  if(direction == "R"){ return("L")}
}
names(mappingListNested) = paste('Node number', str_extract(outList[indicesOfNodes], "[[:digit:]]+"))

columnsToBeEncoded <- ColumnNamesfromColumnNumber
for(col in columnsToBeEncoded){
  if(col %in% primaryColumnstobeDiscarded){
    columnsToBeEncoded <- columnsToBeEncoded[columnsToBeEncoded != col]
  }
}
# Encode  "former" columns where row is "?" from "latter" columns by the order in mappingList
for(col in columnsToBeEncoded){
  # extract column number from former column
  colNumber <- paste(strsplit(col, " ")[[1]][2:4], collapse = " ")
  # Find indices where former column has "?"
  replacementVariables = mappingListNested[[colNumber]]
  #print(replacementVariables)
  for (var in replacementVariables){
    if(grepl("splits as", var)){
    varNameinColumnForm <- paste(c(strsplit(var," ")[[1]][1], colNumber, "surrogate"), collapse = " ")
    #print(col)
    #print(varNameinColumnForm)
    DfOut[, col] = ifelse(
      DfOut[, col] == "-", # which elements are "?"
      DfOut[, varNameinColumnForm], # replace those which are "?" with the values of var
      DfOut[, col]) # otherwise leave unchanged
     }else{
      stringToBeManipulated <-  strsplit(trimws(var), " ")
      stringToBeManipulatedCleaned <- lapply(stringToBeManipulated, function(x) x[!x %in% ""])[[1]]
      columnName <- stringToBeManipulatedCleaned[1]
      #print(columnName)
      value <- stringToBeManipulatedCleaned[3]
      #print(value)
      direction <- stringToBeManipulatedCleaned[6]
      #print(direction)
      mainInfo <- paste(c(columnName, value, direction), collapse = " ")
      #print(mainInfo)
      value2 <- as.numeric(strsplit(mainInfo, " ")[[1]][2])
      #print(value2)
      direction2 <- toupper(strsplit(strsplit(mainInfo, " ")[[1]][3],"")[[1]][1])
      #print(direction2)
      intheDirection <- which((DfOut[,col] == "-") & (DfOut[,columnName] < value2))
      againsttheDirection <- which((DfOut[,col] == "-") & (DfOut[,columnName] >= value2))
      #print(againsttheDirection)
      # change in direction
      DfOut[intheDirection, col] <- direction2
      # change in the opposite direction
      DfOut[againsttheDirection, col] <- oppositeDirection(direction2)

      }
    }
  }
} #else cat("Nothing to encode", sep="\n")


## TEST

# From line list filter all the columns that are under surrogate splits
if(length(lineList) != 0){
  #print( "DOES THIS PART HAPPEN??")
out <- by(unlist(lineList),
          cumsum(grepl("^(Node|Surrogate)", lineList)),
          function(x) {
            if (grepl("Node number.*:", x[1])) x[1] else {
              if (grepl("Surrogate.*:", x[1])) x[-1]
            }
          })

outList <- as.list(do.call(c, unname(out)))

outList <- lapply(outList, function(z){ z[z != ""]})

outList <- Filter(length, outList)

# Create a nested list, where each key is a "number XX" and its elements are the variables needed
indicesOfNodes = which(grepl("Node number", outList))
mappingListNested = lapply(seq_along(indicesOfNodes), function(i){
  # If part is currently added
  if (length(outList) == 1){
    return(outList[(length(outList))])
  }
  else if (i+1 <= length(indicesOfNodes)){
    return(outList[(indicesOfNodes[i]+1):(indicesOfNodes[i+1]-1)])
  } else {
    return(outList[(indicesOfNodes[i]+1):length(outList)])
  }
})
oppositeDirection <- function(direction){
  if(direction == "L"){return("R")}
  if(direction == "R"){ return("L")}
}
names(mappingListNested) = paste('Node number', str_extract(outList[indicesOfNodes], "[[:digit:]]+"))

columnsToBeEncoded <- ColumnNamesfromColumnNumber
for(col in columnsToBeEncoded){
  if(col %in% primaryColumnstobeDiscarded){
    columnsToBeEncoded <- columnsToBeEncoded[columnsToBeEncoded != col]
  }
}
# Encode  "former" columns where row is "?" from "latter" columns by the order in mappingList
for(col in columnsToBeEncoded){
  # extract column number from former column
  colNumber <- paste(strsplit(col, " ")[[1]][2:4], collapse = " ")
  # Find indices where former column has "?"
  replacementVariables = mappingListNested[[colNumber]]
  #print(replacementVariables)
  for (var in replacementVariables){
    if(grepl("splits as", var)){
    varNameinColumnForm <- paste(c(strsplit(var," ")[[1]][1], colNumber, "surrogate"), collapse = " ")
    #print(col)
    #print(varNameinColumnForm)
    DfOutTest[, col] = ifelse(
      DfOutTest[, col] == "-", # which elements are "?"
      DfOutTest[, varNameinColumnForm], # replace those which are "?" with the values of var
      DfOutTest[, col]) # otherwise leave unchanged
     }else{
      stringToBeManipulated <-  strsplit(trimws(var), " ")
      stringToBeManipulatedCleaned <- lapply(stringToBeManipulated, function(x) x[!x %in% ""])[[1]]
      columnName <- stringToBeManipulatedCleaned[1]
      #print(columnName)
      value <- stringToBeManipulatedCleaned[3]
      #print(value)
      direction <- stringToBeManipulatedCleaned[6]
      #print(direction)
      mainInfo <- paste(c(columnName, value, direction), collapse = " ")
      #print(mainInfo)
      value2 <- as.numeric(strsplit(mainInfo, " ")[[1]][2])
      #print(value2)
      direction2 <- toupper(strsplit(strsplit(mainInfo, " ")[[1]][3],"")[[1]][1])
      #print(direction2)
      intheDirection <- which((DfOutTest[,col] == "-") & (DfOutTest[,columnName] < value2))
      againsttheDirection <- which((DfOutTest[,col] == "-") & (DfOutTest[,columnName] >= value2))
      #print(againsttheDirection)
      # change in direction
      DfOutTest[intheDirection, col] <- direction2
      # change in the opposite direction
      DfOutTest[againsttheDirection, col] <- oppositeDirection(direction2)

      }
    }
  }
} #else cat("Nothing to encode", sep="\n")

# Get the name of the columns that are still not encoded
# Get all the primary column names that are in primaryColumnNames
primaryColumnNames <- list()

for(i in names(DfOut)){
  if(grepl("primary",i)){
    primaryColumnNames <- append(primaryColumnNames, i)
  }
}

# Find primary columns that still have "-" after encoding
# Here I used [[1]] to take the value but is this true if we have more than 1 value
columnNumber <- unique(data.frame(which(DfOut[unlist(primaryColumnNames)] == "-", arr.ind = TRUE))["col"])[[1]]
ColumnNamesfromColumnNumber <- names(DfOut[unlist(primaryColumnNames)][columnNumber])

# TEST
# Get the name of the columns that are still not encoded
# Get all the primary column names that are in primaryColumnNames
primaryColumnNamesTest <- list()

for(i in names(DfOutTest)){
  if(grepl("primary",i)){
    primaryColumnNamesTest <- append(primaryColumnNamesTest, i)
  }
}

# Find primary columns that still have "-" after encoding
# Here I used [[1]] to take the value but is this true if we have more than 1 value
columnNumber <- unique(data.frame(which(DfOutTest[unlist(primaryColumnNamesTest)] == "-", arr.ind = TRUE))["col"])[[1]]
ColumnNamesfromColumnNumber <- names(DfOutTest[unlist(primaryColumnNamesTest)][columnNumber])


# ENCODING SURROGATES

# First encode surrogates from categorical primary splits
for(i in seq_len(length(surrogateSplitEncodingsCategorical))) {
    j <- which(DfOut[,names(surrogateSplitEncodingsCategorical)[[i]]] == "-")
    #print(j)
    DfOut[j,names(surrogateSplitEncodingsCategorical)[[i]]] <- DfOut[j,surrogateSplitEncodingsCategorical[[i]]]
}

oppositeDirection <- function(direction){
  if(direction == "L"){return("R")}
  if(direction == "R"){ return("L")}
}

# Second encode surrogates from numerical primary splits
# There are such columns when primary split is absent as well I have not implemented that yet
# For now we have to drop those columns
if(exists("surrogateSplitEncodingsNumerical")){

for(i in seq_len(length(surrogateSplitEncodingsNumerical))) {
    primaryColumn <- strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][1]
    value <- as.numeric(strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][3])
    direction <- toupper(strsplit(strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][4],"")[[1]][1])
    intheDirection <- which((DfOut[,names(surrogateSplitEncodingsNumerical)[i]] == "-") & (DfOut[,primaryColumn] < value))
    againsttheDirection <- which((DfOut[,names(surrogateSplitEncodingsNumerical)[i]] == "-") & (DfOut[,primaryColumn] >= value))
    #value <- as.numeric(strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][3])
    #direction <- toupper(strsplit(strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][4],"")[[1]][1])
    # change in direction
    DfOut[intheDirection, names(surrogateSplitEncodingsNumerical)[i]] <- direction
    # change in the opposite direction
    DfOut[againsttheDirection, names(surrogateSplitEncodingsNumerical)[i]] <- oppositeDirection(direction)
}
} else cat("Variable does not exist", sep="\n")


##TEST
# First encode surrogates from categorical primary splits
for(i in seq_len(length(surrogateSplitEncodingsCategorical))) {
    j <- which(DfOutTest[,names(surrogateSplitEncodingsCategorical)[[i]]] == "-")
    #print(j)
    DfOutTest[j,names(surrogateSplitEncodingsCategorical)[[i]]] <- DfOutTest[j,surrogateSplitEncodingsCategorical[[i]]]
}

oppositeDirection <- function(direction){
  if(direction == "L"){return("R")}
  if(direction == "R"){ return("L")}
}

# Second encode surrogates from numerical primary splits
# There are such columns when primary split is absent as well I have not implemented that yet
# For now we have to drop those columns
if(exists("surrogateSplitEncodingsNumerical")){

for(i in seq_len(length(surrogateSplitEncodingsNumerical))) {
    primaryColumn <- strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][1]
    value <- as.numeric(strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][3])
    direction <- toupper(strsplit(strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][4],"")[[1]][1])
    intheDirection <- which((DfOutTest[,names(surrogateSplitEncodingsNumerical)[i]] == "-") & (DfOutTest[,primaryColumn] < value))
    againsttheDirection <- which((DfOutTest[,names(surrogateSplitEncodingsNumerical)[i]] == "-") & (DfOutTest[,primaryColumn] >= value))
    #value <- as.numeric(strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][3])
    #direction <- toupper(strsplit(strsplit(surrogateSplitEncodingsNumerical[[i]], " ")[[1]][4],"")[[1]][1])
    # change in direction
    DfOutTest[intheDirection, names(surrogateSplitEncodingsNumerical)[i]] <- direction
    # change in the opposite direction
    DfOutTest[againsttheDirection, names(surrogateSplitEncodingsNumerical)[i]] <- oppositeDirection(direction)
}
} else cat("Variable does not exist", sep="\n")


# Check which columns still have "-"

ColumnsIWant <- list()

for(i in names(DfOut)){
  if(grepl("*primary$",i) || grepl("*surrogate$",i) ){
    ColumnsIWant <- append(ColumnsIWant, i)
  }
}

# Check if the final dataframe has "-"
dfThatHasMissing <- data.frame(which(DfOut[unlist(ColumnsIWant)] == "-", arr.ind = TRUE))

# Find list of columns which have "-"
columnNumbersThatHave <- unique(dfThatHasMissing[["col"]])

multipleEncodingColumnstoBeDiscarded <- list()
for(i in columnNumbersThatHave){
  multipleEncodingColumnstoBeDiscarded <- append(multipleEncodingColumnstoBeDiscarded, names(DfOut[unlist(ColumnsIWant)][i]))
}
DfOutCopy <- DfOut 
# Discard those columns
DfOut <- DfOut[ , !names(DfOut) %in% multipleEncodingColumnstoBeDiscarded]

ColumnsIWant <- list()

for(i in names(DfOut)){
  if(grepl("*primary$",i) || grepl("*surrogate$",i) ){
    ColumnsIWant <- append(ColumnsIWant, i)
  }
}

#print(ColumnsIWant)

nodes <- DfOut[unlist(ColumnsIWant)]
## SCENARIO 1
scenario1Name <- paste0(datasetName, "_imit_splits_scenario_1.csv", collapse = "")
# Convert to csv file
write.csv(nodes, scenario1Name, row.names = FALSE)


## TEST
# Check which columns still have "-"

ColumnsIWantTest <- list()

for(i in names(DfOutTest)){
  if(grepl("*primary$",i) || grepl("*surrogate$",i) ){
    ColumnsIWantTest <- append(ColumnsIWantTest, i)
  }
}

# Check if the final dataframe has "-"
#print("DOES THIS PART HAPPEN")
dfThatHasMissingTest <- data.frame(which(DfOutTest[unlist(ColumnsIWantTest)] == "-", arr.ind = TRUE))
#print("WHAT HAPPENED")
# Find list of columns which have "-"
columnNumbersThatHaveTest <- unique(dfThatHasMissingTest[["col"]])

multipleEncodingColumnstoBeDiscardedTest <- list()
for(i in columnNumbersThatHaveTest){
  multipleEncodingColumnstoBeDiscardedTest <- append(multipleEncodingColumnstoBeDiscardedTest, names(DfOutTest[unlist(ColumnsIWantTest)][i]))
}

# Discard those columns
DfOutTestCopy <- DfOutTest
DfOutTest <- DfOutTest[ , !names(DfOutTest) %in% multipleEncodingColumnstoBeDiscarded]

ColumnsIWantTest <- list()

for(i in names(DfOutTest)){
  if(grepl("*primary$",i) || grepl("*surrogate$",i) ){
    ColumnsIWantTest <- append(ColumnsIWantTest, i)
  }
}

nodesTest <- DfOutTest[unlist(ColumnsIWantTest)]
## SCENARIO 1
scenario1Name <- paste0(datasetName, "_imit_splits_scenario_1_test.csv", collapse = "")
# Convert to csv file
write.csv(nodesTest, scenario1Name, row.names = FALSE)


if(dim(primaryDiscardedDf)[2] != 0){
## SCENARIO 2a)
#print(length(names(primaryDiscardedDf)))
#print(length(names(Filter(function(x) length(unique(na.omit(unlist(x)))) > 1, primaryDiscardedDf))))
dummy <- dummyVars(" ~ .", data = primaryDiscardedDf)
primaryDiscardedDf_OHE <- data.frame(predict(dummy, newdata = primaryDiscardedDf))
scenario2Name <- paste0(datasetName, "_imit_scenario_2_a.csv", collapse = "")
scenario_2_a <- cbind(nodes, primaryDiscardedDf_OHE)
write.csv(scenario_2_a, scenario2Name, row.names = FALSE)

## TEST
#print(sum(is.na(primaryDiscardedDfTest)))
#print(length(names(primaryDiscardedDfTest)))
#print(length(names(Filter(function(x) length(unique(na.omit(unlist(x)))) > 1, primaryDiscardedDfTest))))
primaryDiscardedDfTest <- Filter(function(x) length(unique(na.omit(unlist(x)))) > 1, primaryDiscardedDfTest)
dummy <- dummyVars(" ~ .", data = primaryDiscardedDfTest)
primaryDiscardedDfTest_OHE <- data.frame(predict(dummy, newdata = primaryDiscardedDfTest))
scenario2Name <- paste0(datasetName, "_imit_scenario_2_a_test.csv", collapse = "")
scenario_2_aTest <- cbind(nodesTest, primaryDiscardedDfTest_OHE)
write.csv(scenario_2_aTest, scenario2Name, row.names = FALSE)

## SCENARIO 2b)
necessaryOHEColumns <- list()
for(i in names(primaryDiscardedDf_OHE)){
  if((grepl("R$", i) || grepl("L$", i))){
    necessaryOHEColumns <- append(necessaryOHEColumns, i)
 }
}
primaryDiscardedDf_OHE_RL <- primaryDiscardedDf_OHE[unlist(necessaryOHEColumns)]

scenario_2_b <- cbind(nodes, primaryDiscardedDf_OHE_RL)
scenario3Name <- paste0(datasetName, "_scenario_2_b.csv", collapse = "")
write.csv(scenario_2_b, scenario3Name, row.names = FALSE)


## TEST
necessaryOHEColumnsTest <- list()
for(i in names(primaryDiscardedDfTest_OHE)){
  if((grepl("R$", i) || grepl("L$", i))){
    necessaryOHEColumnsTest <- append(necessaryOHEColumnsTest, i)
 }
}
primaryDiscardedDf_OHE_RLTest <- primaryDiscardedDfTest_OHE[unlist(necessaryOHEColumnsTest)]

scenario_2_bTest <- cbind(nodesTest, primaryDiscardedDf_OHE_RLTest)
scenario3Name <- paste0(datasetName, "_scenario_2_b_test.csv", collapse = "")
write.csv(scenario_2_bTest, scenario3Name, row.names = FALSE)
} else{cat("There are no primary columns to be discarded as a result scenario a) and b) can't be processed",sep="\n")}
}