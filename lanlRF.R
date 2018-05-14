source("src/auxiliar.R")
source("src/utils.R")
library(caret)
os <- get_os()
if(os == "osx" | os == "linux"){
  library(doMC)
}else{
  library(doParallel)
}
### Carga de datos----
lanl <- readRDS("lanl.rds")


lanlEvents <- c("18-Hardware-CPU",
                "2-Hardware-Memory Dimm",
                "2-Software-DST",
                "16-Undetermined-Unresolvable",
                "16-Missing-NA")

lanl$fw$f.5m <- lanl$fw$f.5m[,which(names(lanl$fw$f.5m) %in% lanlEvents)]  

### Funciones de modelado ----
EvaluateModels <- function(input, output){
  
  if(os == "osx" | os == "linux"){
    registerDoMC()
  }else{
    registerDoParallel()
  }
  
  # Random Forest
  set.seed(7777)
  print(Sys.time())
  print("Starting Random Forest")
  trControl <- trainControl(method =  "repeatedcv", number = 10, repeats = 3, search = "random")
  RFtime <- system.time({RFmodel <- tryCatch(expr = train(x = input, y = output, method = "ranger",
                                                          metric = "Kappa", trControl = trControl, tuneLength = 100),
                                             error = function(e){
                                               return(NA)
                                             })})
  
  
  return(list(RFmodel, RFtime))
}

lanlModelsRF <- list()    
for(i in 1:5){
  print(paste("Starting round", i, sep = " "))
  lanlModelsRF[[i]] <- EvaluateModels(input = lanl$back$b.5m[,-1],
                                      output = factor(ifelse(test = lanl$fw$f.5m[,i] > 0,yes = "yes", no = "no")))
  lanlModelsRF[[i]][[1]]$trainingData <- NULL
  saveRDS(lanlModelsRF, file = "lanlModelsRF.rds")
}




