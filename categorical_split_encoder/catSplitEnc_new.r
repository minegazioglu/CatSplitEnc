catSplitEncoding <- function(targetVariable, trainData = train, testData = test, problemType = "classification", datasetName = datasetName, catVariables = catVariables){
  # Fit rpart depending on the problem type : classification or regression
  # maxdepth 7 and cp 0
  # multiclass i�in dogru mu yapiyoruz?
  
  datasetNameTxt <- paste0(datasetName, ".txt", collapse = "")
  
  if(problemType == "regression"){
    # Fit rpart
    tree_fit = rpart(formula, trainData, method = 'anova', control = rpart.control(cp = 0, maxdepth = 4, usesurrogate = 2))
  } else if(problemType %in% c("multiclass", "classification")){
    tree_fit = rpart(formula, trainData, method = 'class', control = rpart.control(cp = 0, maxdepth = 4, usesurrogate = 2))
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
  
  #print("is this the part")
  primaryfullNames <- stack(Filter(Negate(is.null),
                                   lapply(split(necessaryLines, cumsum(grepl('Node number', necessaryLines))), 
                                          function(x) {
                                            x1 <- sub(",.*", "", x[grep('improve', x)])
                                            if(length(x1) > 0) paste(x1, strsplit(x[1],":")[[1]][1])
                                          })))[2:1][[2]]
  
  #print("nooooooo")
  primaryfullNamesModified <- list()
  
  for(name in primaryfullNames){
    primaryfullNamesModified <- append(primaryfullNamesModified, paste(c(strsplit(name, "\\s+")[[1]][c(1,5,6,7)],"primary"), collapse = " "))
  }
  
  
  primaryInfo <- list()
  primarygeneralName <- list()
  primaryPattern <- "splits as\\s*(.*?)\\s,*improve"
  
  for(i in necessaryLines){
    if(grepl("improve",i)){
      name <- strsplit(i, "\\s+")[[1]][1]
      primarygeneralName <- append(primarygeneralName, name)
      primaryInfo <- append(primaryInfo, trimws(sub(",","",regmatches(i, regexec(primaryPattern, i))[[1]][2])))
    }
  }
  
  primaryfullNamesModified <- unlist(primaryfullNamesModified)
  
  primaryInfo <- unlist(primaryInfo)
  
  names(primaryInfo) <- unlist(primaryfullNamesModified)
  
  
  primaryInfo <- lapply(split(x = primaryInfo, f = sapply(strsplit(names(primaryInfo)," ") , "[", 1)), unlist)
  
  #print("magia")
  # SURROGATE SPLITS
  
  if(length(unlist(lapply(split(necessaryLines, cumsum(grepl('Node number', necessaryLines))), 
                          function(x) {
                            x1 <- sub(",.*", "", x[grep('agree', x)])
                            if(length(x1) > 0) paste(x1, strsplit(x[1],":")[[1]][1])
                          }))) > 0){
    
    surrogatefullNames <- stack(Filter(Negate(is.null),
                                       lapply(split(necessaryLines, cumsum(grepl('Node number', necessaryLines))), 
                                              function(x) {
                                                x1 <- sub(",.*", "", x[grep('agree', x)])
                                                if(length(x1) > 0) paste(x1, strsplit(x[1],":")[[1]][1])
                                              })))[2:1][[2]]
    
    
    #print("bluio")
    
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
  }
  
  
  #print("ratia")
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
  #####------------------------------------------------------------------------------------------
  
  # Convert Primary and Surrogate Splits into Separate Dataframes
  
  if (exists("surrogateInfo") > 0){
    #print("should not be printed")
    #print(surrogateInfo)
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
    
  }
  
  #print(surrogateOut)
  # Unite Primary and Surrogate Dfs
  if (exists("surrogateOut")){
    DfOut <- merge(primaryOut, surrogateOut)
    DfOutTest <- merge(primaryOutTest, surrogateOutTest)} else {
      DfOut <- primaryOut
      DfOutTest <- primaryOutTest}
  
  # Filter out duplicate columns 
  DfOut <- DfOut[, !duplicated(colnames(DfOut))]
  DfOut <- DfOut[order(DfOut$id), ]
  #Test
  DfOutTest <- DfOutTest[, !duplicated(colnames(DfOutTest))]
  DfOutTest <- DfOutTest[order(DfOutTest$id), ]
  
  
  ### ----------------------------------------------------------------------------
  # Check which columns still have "-"
  ColumnsIWant <- list()
  
  for(i in names(DfOut)){
    if(grepl("*primary$",i) || grepl("*surrogate$",i) ){
      ColumnsIWant <- append(ColumnsIWant, i)
    }
  }
  nodes <- DfOut[unlist(ColumnsIWant)]
  
  # Check if the final dataframe has "-"
  dfThatHasMissing <- data.frame(which(nodes[unlist(ColumnsIWant)] == "-", arr.ind = TRUE))
  
  # Find list of columns which have "-"
  columnNumbersThatHave <- unique(dfThatHasMissing[["col"]])
  columnNumbersThatHaveNames <- names(nodes)[columnNumbersThatHave]
  
  #print(columnNumbersThatHaveNames)
  
  DfMissingLevel <- DfOut[,columnNumbersThatHaveNames]
  
  nodes <- nodes[ , !(names(nodes) %in% columnNumbersThatHaveNames)]
  
  ## TEST
  # Check which columns still have "-"
  
  ColumnsIWantTest <- list()
  
  for(i in names(DfOutTest)){
    if(grepl("*primary$",i) || grepl("*surrogate$",i) ){
      ColumnsIWantTest <- append(ColumnsIWantTest, i)
    }
  }
  
  
  
  nodesTest <- DfOutTest[unlist(ColumnsIWantTest)]
  
  # Check if the final dataframe has "-"
  dfThatHasMissingTest <- data.frame(which(nodesTest[unlist(ColumnsIWantTest)] == "-", arr.ind = TRUE))
  # Find list of columns which have "-"
  columnNumbersThatHaveTest <- unique(dfThatHasMissingTest[["col"]])
  
  columnNumbersThatHaveNamesTest <- names(nodesTest)[columnNumbersThatHaveTest]
  
  DfMissingLevelTest <- DfOutTest[,columnNumbersThatHaveNamesTest]
  
  nodesTest <- nodesTest[ , !(names(nodesTest) %in% columnNumbersThatHaveNamesTest)]
  
  
  ## SCENARIO 2a)
  # TRAIN
  if(dim(DfMissingLevel)[2] > 0) {
    dummy <- dummyVars(" ~ .", data = DfMissingLevel)
    primaryDiscardedDf_OHE <- data.frame(predict(dummy, newdata = DfMissingLevel))
    #print(head(primaryDiscardedDf_OHE))
    
    colsWMissingTrain <- c()
    for(col in names(primaryDiscardedDf_OHE)){
      if(endsWith(col, ".")){
        colsWMissingTrain <- append(colsWMissingTrain, col)
      }
    }
    
    scenario2Name <- paste0(datasetName, "_imit_scenario_2_a.csv", collapse = "")
    scenario_2_a <- cbind(nodes, primaryDiscardedDf_OHE)
    
    scenario_2_a <- scenario_2_a[ , !(names(scenario_2_a) %in% colsWMissingTrain)]
    
    #scenario_2_a <- nodes
    write.csv(scenario_2_a, scenario2Name, row.names = FALSE)
  } else {
    
    
    scenario2Name <- paste0(datasetName, "_imit_scenario_2_a.csv", collapse = "")
    scenario_2_a <- nodes
    
    #scenario_2_a <- scenario_2_a[ , !(names(scenario_2_a) %in% colsWMissingTrain)]
    
    #scenario_2_a <- nodes
    write.csv(scenario_2_a, scenario2Name, row.names = FALSE)
    
  }
  
  
  
  
  ## TEST
  #primaryDiscardedDfTest <- Filter(function(x) length(unique(na.omit(unlist(x)))) > 1, primaryDiscardedDfTest)
  if(dim(DfMissingLevelTest)[2] > 0) {
    
    
    DfMissingLevelTest <- Filter(function(x)(length(unique(x))>1), DfMissingLevelTest)
    
    
    print(dim(DfMissingLevelTest)[2])
    
    dummy <- dummyVars(" ~ .", data = DfMissingLevelTest)
    primaryDiscardedDfTest_OHE <- data.frame(predict(dummy, newdata = DfMissingLevelTest))
    
    colsWMissingTest <- c()
    for(col in names(primaryDiscardedDfTest_OHE)){
      if(endsWith(col, ".")){
        colsWMissingTest <- append(colsWMissingTest, col)
      }
    }
    
    
    scenario2Name <- paste0(datasetName, "_imit_scenario_2_a_test.csv", collapse = "")
    scenario_2_aTest <- nodesTest
    
    scenario_2_aTest <- scenario_2_aTest[ , !(names(scenario_2_aTest) %in% colsWMissingTest)]
    
    write.csv(scenario_2_aTest, scenario2Name, row.names = FALSE)  
  } else {
    
    
    
    scenario2Name <- paste0(datasetName, "_imit_scenario_2_a_test.csv", collapse = "")
    scenario_2_aTest <- nodesTest
    
    #scenario_2_aTest <- scenario_2_aTest[ , !(names(scenario_2_aTest) %in% colsWMissingTest)]
    #scenario_2_aTest <- nodesTest
    #columnsNamesThatEndWithMissing = 
    write.csv(scenario_2_aTest, scenario2Name, row.names = FALSE)  
  }
  
  
}