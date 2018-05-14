library(caret)
library(plyr)
library(dplyr)

CreateRFModels <- function(dfList){
  modelList <- vector("list",length(dfList)) 
  names(modelList) <- names(dfList)
  for(i in 1:length(dfList)){
    print(i)
    modelList[[i]] <- TrainModels(dfList[[i]])
  }
  return(modelList)
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

TrainModels <- function(dat){
  model <- tryCatch(
    {TrainModel(df = dat)}
    , error = function(error)
    {return(NA)})
  return(model)
}

TrainModel <- function(df){
  df <- df[,-1]
  df2 <- PrepareForSmote(df)
  levels(df2$target) <- c("no", "yes")
  fitControl <- trainControl(## 10-fold ∫CV
    method = "cv",
    number = 5,
    sampling = "smote",
    summaryFunction = twoClassSummary,
    classProbs = TRUE)
  x <- select(df2, -target)
  y <- df2$target
  rfModel <- train(x = x,
                   y = y,
                   data = df2,
                   method = "rf",
                   trControl = fitControl,
                   metric = "ROC",
                   tuneGrid=data.frame(mtry = floor(sqrt(dim(x)[2]))),
                   importance = TRUE)
  return(rfModel)
}
