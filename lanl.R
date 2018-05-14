# Carga de lanl y librerías ----
library(plyr)
library(tidyverse)
library(readr)
library(lubridate)

lanl <- as.data.frame(read_csv(file = "dat/datLANL/lanl.csv", skip = 1))
# Preproceso sipmle de variables 
lanl <- select(lanl, c(1,3,4,6, 8:10, 17,20:25))
lanl$System <- factor(lanl$System)
lanl$nodes <- factor(lanl$nodes)
lanl$procstot <- factor(lanl$procstot)
lanl$`Prob Started (mm/dd/yy hh:mm)` <- mdy_hm(lanl$`Prob Started (mm/dd/yy hh:mm)`)
names(lanl)[1:8] <- c("system", "nodes", "procs", "nodenum", "nodeinstall", "nodeprod", "nodedecom", "date")

#Apaño las fechas raras que no entiendo por qué tienen que poner en ese formato
lanl$nodeinstall[lanl$nodeinstall == "before tracking"] <- "95-Jan"
lanl$nodeprod[lanl$nodeprod == "before tracking"] <- "95-Jan"
lanl$nodedecom[lanl$nodedecom == "current"] <- "05-Oct"
lanl$nodeinstall <- parse_date(x = lanl$nodeinstall, format = "%y-%b", locale = locale("en"))
lanl$nodeprod <- parse_date(x = lanl$nodeprod, format = "%y-%b", locale = locale("en"))
lanl$nodedecom <- parse_date(x = lanl$nodedecom, format = "%y-%b", locale = locale("en"))


# Procesamiento de variables de error
lanl$errorType <- NA
lanl$subtype <- NA
colStart <- which(names(lanl) == "Facilities")
for(i in 1:dim(lanl)[1]){
  notNA <- which(!is.na(lanl[i,colStart:(colStart+5)]))+colStart-1
  if(length(notNA) > 0){
    lanl$errorType[i] <- names(lanl)[notNA]
    lanl$subtype[i] <- as.character(lanl[i,notNA])
  }
}
lanl$errorType[is.na(lanl$errorType)] <- "Missing"
lanl$subtype[is.na(lanl$errorType)] <- "Missing"
lanl <- select(lanl, c(1:(colStart-1), (colStart+6):(colStart+7)))
lanl$errorType <- factor(lanl$errorType)

# Creación de variables -----

lanl$type <- factor(paste(lanl$system, lanl$errorType, lanl$subtype, sep = "-"))
View(lanl %>% group_by(Event_Type) %>% summarise(count = n()))
names(lanl)[8] <- c("date")

evTable<-lanl %>% tbl_df() %>% select(date,type)
evTable<-evTable[order(evTable$date),]
winEvTable <-getWinEvTables(evTable, backward = list(b.5m=25000), forward = list(f.5m=10800))
lanl <- winEvTable
saveRDS(lanl, file = "lanl.rds")
