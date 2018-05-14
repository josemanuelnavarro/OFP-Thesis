require(randomForest)

CreateModels <- function(dfs, complexOutput = FALSE, rounds = 1, trimZeros = FALSE, zeroProportion = 10){
  list_of_models        <- vector("list",length(dfs))         
  names(list_of_models) <- names(dfs)
  for (event in 1:length(dfs)){
    print(paste("Starting event", event, "out of", length(dfs)))
    list_of_models[[event]] <- Model(dfs[[event]][,-1], rounds = rounds, complexOutput = complexOutput, trimZeros = trimZeros, zeroProportion = zeroProportion)
    print(list_of_models[[event]])
  }
  return(list_of_models)
}

Model <- function(df, rounds = 1, complexOutput = FALSE, trimZeros = FALSE, zeroProportion = 3){
  result<-tryCatch(
{
  return(TrainAndFit(df, rounds = rounds, complexOutput = complexOutput, trimZeros = trimZeros, zeroProportion = zeroProportion))
},
error=function(error){
  if (complexOutput){
    return(list("confusion_matrix"=NaN,"precision"=NaN,"recall"=NaN,"fscore"=NaN,"model"=NULL))
  }else{
    return(NaN)
  }
  })
return(result)
}

TrainAndFit <- function(df, rounds = 1, complexOutput = complexOutput, trimZeros = FALSE, zeroProportion = 3){
  if(complexOutput){
    modelList <- list()
    for (i in 1:rounds){
      dat            <- StratifyDf(df, trimZeros = trimZeros, zeroProportion = zeroProportion)
      train_in       <- dat$train[,-ncol(dat$train)]
      train_out      <- dat$train[, ncol(dat$train)]
      test_in        <- dat$test[,-ncol(dat$test)]
      test_out       <- dat$test[, ncol(dat$test)]
      model          <- Train(train_in, train_out)
      modelList[[i]] <- Fit(model, test_in, test_out, TRUE)
    }
    return(modelList)
  } else{
    fscore <- numeric()
    for (i in 1:rounds){
      dat       <- StratifyDf(df, trimZeros = trimZeros, zeroProportion = zeroProportion)
      train_in       <- dat$train[,-ncol(dat$train)]
      train_out      <- dat$train[, ncol(dat$train)]
      test_in        <- dat$test[,-ncol(dat$test)]
      test_out       <- dat$test[, ncol(dat$test)]
      model          <- Train(train_in, train_out)
      fscore[i]      <- Fit(model, test_in, test_out, FALSE)
    }
    return(fscore)
  }
}

Train<-function(train_in,train_out){
    input_matrix<-matrix(nrow=dim(train_in)[1],ncol=dim(train_in)[2])
    input_matrix[which(train_in == TRUE)]  <- 1
    input_matrix[which(train_in == FALSE)] <- 0
    output_matrix <- matrix(nrow=length(train_out),ncol=1)
    output_matrix[which(train_out == TRUE)]  <- 1
    output_matrix[which(train_out == FALSE)] <-0
    tryCatch({model<-randomForest(x=input_matrix,y=factor(output_matrix), ntree = 50)})
  return(model)
}


Fit <- function(model, test_in, test_out, returnList = FALSE){
  input  <- test_in
  output <- test_out
  
  input_matrix                      <- matrix(nrow=dim(input)[1],ncol=dim(input)[2])
  input_matrix[which(input==TRUE)]  <- 1
  input_matrix[which(input==FALSE)] <- 0
  
  output_vec                       <- numeric()
  output_vec[which(output==TRUE)]  <- 1
  output_vec[which(output==FALSE)] <- 0
  output_vec <- factor(output_vec)
  
  prediction <- predict(model,newdata = input_matrix)
  test_table <- data.frame(pred=prediction,real=output_vec)
  
  confusion_matrix<-matrix(nrow=2,ncol=2)
  confusion_matrix[1,1]<-length(which(test_table[,1]==1&test_table[,2]==1))
  confusion_matrix[2,1]<-length(which(test_table[,1]==0&test_table[,2]==1))
  confusion_matrix[1,2]<-length(which(test_table[,1]==1&test_table[,2]==0))
  confusion_matrix[2,2]<-length(which(test_table[,1]==0&test_table[,2]==0))
  
  if (sum(confusion_matrix[1,])==0){
    precision<-0
  }else{
    precision<-confusion_matrix[1,1]/sum(confusion_matrix[1,])}
  
  if (sum(confusion_matrix[1,])==0){
    recall<-0
  }else{recall<-confusion_matrix[1,1]/sum(confusion_matrix[,1])}
  fscore<-2*precision*recall/(precision+recall)
  if (returnList){
  things_to_return<-list("confusion_matrix"=confusion_matrix,"precision"=precision,"recall"=recall,"fscore"=fscore,"model"=model)
  return(things_to_return)
  }else{
    return(fscore)
  }  
}

