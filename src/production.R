CreateFinalModels <- function(dfList){
  modelList <- vector("list",length(dfList)) 
  names(modelList) <- names(dfList)
  for(i in 1:length(dfList)){
    print(i)
    modelList[[i]] <- CreateFinalModel(dfList[[i]])
  }
  return(modelList)
}

CreateFinalModel <- function(dat){
  require(randomForest)
  dat <- dat[,-1] #remove date field
  dat <- Smotify(dat)
  input <- dat[,1:(dim(dat)[2]-1)]
  #model <- randomForest( x = input, y = dat$target, importance = TRUE, ntree = 50)
  model <- tryCatch(
    {randomForest( x = input, y = dat$target, importance = TRUE, ntree = 50)}
    , error = function(error)
    {return(NA)})
return(model)
}

Smotify <- function(df){
  library(DMwR)
  smoteDF <- PrepareForSmote(df)
  return(SMOTE(form = target ~ ., data = smoteDF))
}

PrepareForSmote <- function(df2){
  #df2$target <- factor(df2$target > 0)
  newDF <- data.frame(matrix(nrow = dim(df2)[1], ncol = dim(df2)[2]))
  for(i in 1:dim(newDF)[2]){
    #newDF[,i] <- factor(unlist(df2[,i]))
    newDF[,i] <- unlist(df2[,i])
  }
  names(newDF) <- names(df2)
  newDF$target <- factor(newDF$target > 0 )
  return(newDF)
}
