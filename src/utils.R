require(lubridate)
require(plyr)
require(dplyr)

correctDates<-function(df){
  df$Created_On<-gsub("ene","01",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("feb","02",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("mar","03",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("abr","04",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("may","05",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("jun","06",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("jul","07",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("ago","08",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("sep","09",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("oct","10",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("nov","11",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("dic","12",substring(as.character(df$Created_On),0,20))
  df$Created_On<-dmy_hms(df$Created_On,tz="Europe/Madrid")
  
  return(df)
}

LoadFullData <- function(data = "Totta"){
  files <- list.files(path = paste(getwd(),paste("/dat/dat", data, "/", sep = ""), sep = ""))
  dat <- list()
  for(i in 1:length(files)){
    dat[[i]] <- read.table(paste(getwd(),paste("/dat/dat", data,"/", sep = ""), files[i],sep=""), sep=",")
  }
  dat <- rbind.fill(dat)
  names(dat) <- c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
  dat<-correctDates(dat)
  levels(dat$Severity)[levels(dat$Severity)==""]<-"Blank"
  for (i in 1:nrow(dat)){
    event_number <- dat$Event_Type[i]
    if (event_number == "4293918760"){
      hex_number <- "0xFFF00028"
    }else if (event_number == "4293955703"){
      hex_number <- "0xFFF09077"
    }else if (event_number == "4293955704"){
      hex_number <- "0xFFF09078"
    }else if (event_number == "3288365312"){
      hex_number <-  "0xC4007900"
    }else if (event_number == "4293918944"){
      hex_number <- "0xFFF000E0"
    }else if (event_number == "4293918809"){
      hex_number <- "0xFFF00059"
    }else{
      hex_number <- paste("0x",as.hexmode(as.numeric(event_number)),sep="")
    }
    dat$Event_Type[i] <- hex_number
  }
  dat$Event_Type<-factor(paste(dat$Name,dat$Event_Type,sep="-"))
  eventData <- EventData(dat)
  save(eventData, file="event_data.Rdata")
  dat <- reduce_data(dat)
  evTable<-dat %>% tbl_df() %>% select(date,type)
  evTable<-evTable[order(evTable$date),]
  winEvTable <-getWinEvTables(evTable)
  #predTables <- getPredTables(winEvTable$back,winEvTable$fw)
  return(winEvTable)
}

reduce_data <- function(dat){
 rdata <- data.frame("date" = dat$Created_On, "node" = droplevels(dat$Name), "type"=droplevels(dat$Event_Type)) 
}

LogicalToFactor <- function(vec){
  result <- numeric(length(vec))
  result[which(vec == TRUE)] <- 1
  result[which(vec == FALSE)] <- 0
  return(result)
}

StratifyDf <- function(df, train_rate = 0.99, trimZeros = FALSE, zeroProportion = 3){
  result <- list()
  ones  <- df[df$target == 1,]
  zeros <- df[df$target == 0,]
  ones  <- ones[sample(nrow(ones)),]
  zeros <- zeros[sample(nrow(zeros)),]
  if (trimZeros){
    limit <- round(dim(ones)[1]*train_rate)
    if (limit > dim(zeros)[1]){
      result$train <- rbind(ones[1:limit,], zeros)
    } else{
      result$train <- rbind(ones[1:limit,], zeros[1:(limit * zeroProportion),])
    }
  } else{
    result$train <- rbind(ones[1:round(dim(ones)[1]*train_rate),], zeros[1:round(dim(zeros)[1]*train_rate),])
  }
  result$test  <- rbind(ones[round(dim(ones)[1]*train_rate + 1) : dim(ones)[1],], zeros[round(dim(zeros)[1]*train_rate + 1) : dim(zeros)[1],])
  result$train <- result$train[sample(nrow(result$train)),]
  result$test  <- result$test[sample(nrow(result$test)),]
  return(result)
}

RecodeFactors<-function(df){
  for (i in 1:length(levels(df$type))){
    levels(df$type)[i]<-as.character(i)
  }
  return(df)
}

EventData <- function(dat){
  dat$Event <- as.character(dat$Event)
  events <- unique(dat$Event_Type)
  result <- data.frame(character(length(events)), character(length(events)), numeric(length(events)),  character(length(events)), stringsAsFactors = FALSE)
  names(result) <- c("event", "severity", "times", "message")
  result[,1] <- events
  levels(result[,2]) <- ordered(c("Blank", "Minor", "Major", "Critical"))
  for (i in 1:nrow(result)){
    result[i,2] <- as.character(dat$Severity[which(dat$Event_Type == events[i])[1]])
    result[i,3] <- length(which(dat$Event_Type == events[i]))
    result[i,4] <- dat$Event[which(dat$Event_Type == events[i])[1]]
  }
  return(arrange(result, desc(severity)))
}


