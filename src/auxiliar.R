require(plyr)
require(dplyr)
require(lubridate)
require(ggplot2)
require(reshape2)
#require(doParallel)

#usage
#getEventsInWindow(date,.data,300, "backward")

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


getEventsInWindow<-function(windowOrigin,data,windowSize=300,direction="forward", offset=0)
{
    # filter: events before or after window origin
  data.filtered<-data.frame()
    # check window origin offset 
  if(is.null(offset) || offset<0){offset<-0}
  if(offset>windowSize){offset<-windowSize}
  if(direction=="forward")
  {data.filtered<-data %>% filter (date>windowOrigin+offset & date <= windowOrigin+windowSize)}else 
  if(direction=="backward")
  {data.filtered<-data %>% filter (date<=windowOrigin-offset & date >= windowOrigin-windowSize)}else 
  if(direction=="both")
  {data.filtered<-data %>% filter (date<=windowOrigin+windowSize & date >= windowOrigin-windowSize)}
    # if empty window, return row marking this date as having no events in window  
  if(nrow(data.filtered)==0) {return(data.frame(date=windowOrigin, type="NO.EVENTS", evInWin=TRUE))}
    #if (countEvents) result <- data.filtered %>% group_by(type) %>%  summarise(minDist1=n())
  # result<-data.filtered %>% group_by(type) %>% summarise(evInWin=n()>0) %>% mutate(date=windowOrigin) %>% select(date,type,evInWin)
  result<-data.filtered %>% group_by(type) %>% summarise(evInWin=n()) %>% mutate(date=windowOrigin) %>% select(date,type,evInWin)
}
applyGetEventsInWindow<-function(data,windowSize=300,direction="forward", offset=0)
{
  tmp<-ldply(unique(data$date),getEventsInWindow,data, windowSize=windowSize,direction=direction, offset=offset) 
  #dcast(tmp, date ~ type, fun.aggregate= any) %>% tbl_df()
  dcast(tmp, date ~ type, fun.aggregate= sum) %>% tbl_df()
}
getWinEvTables <-function(dat,
                          backward=list(b.5m=5*60),
                          forward=list(f.5m=5*60), 
                          offset.back=list(),
                          offset.fw=list())
{
  back<-llply.parallel.multilist(list.ref=backward,
                                 list.multi=list(windowSize=backward, offset=offset.back),
                                 n=1,
                                 dat=dat,
                                 .fun=function(sizeOffsetList,dat)
                                 {applyGetEventsInWindow(dat,windowSize=sizeOffsetList$windowSize, offset=sizeOffsetList$offset,direction="backward")}
  )
  
  attr(back,"windowSize")<-backward
  attr(back,"offset")<-offset.back
  
  fw<-llply.parallel.multilist(list.ref=forward,
                               list.multi=list(windowSize=forward, offset=offset.fw),
                               n=1,
                               dat,
                               .fun=function(sizeOffsetList,dat)
                               {applyGetEventsInWindow(dat,windowSize=sizeOffsetList$windowSize, offset=sizeOffsetList$offset,direction="forward")}
  )
  
  attr(fw,"windowSize")<-forward
  attr(fw,"offset")<-offset.fw
  
  list(back=back,fw=fw)
}
getPredTables <- function (back,fw)
{
  llply.cross.ab(back, fw, .fun2=function(back.1,fw.1)
  {
    llply (fw.1 %>% select(-date),back.1,.fun=function(fw.1.col,back.1)
    {cbind(back.1,target=fw.1.col) %>% tbl_df()}
    )}
  )
}
getTypeDistances<-function(index,data,maxDist=24*3600,direction="forward",method="eventDist")
{
  #data<-evTableED
  tmpResult1<-data %>% mutate(dist1=as.numeric(as.duration(date[index]%--%date))) 
  win<-switch(direction,
              forward=(index:nrow(data))[-1], 
              backward=(1:index)[-index], 
              both=-index)
  tmpResult2<- tmpResult1[win,] %>% group_by(type) 
  # table of type levels
  levelTable<-tbl_df(data.frame(type=levels(tmpResult1$type)))  
  # Result to return in case of empty data
  resultEmpty<-levelTable %>% 
    mutate(minDist1=switch(method,
                           anyEvent=FALSE,
                           eventDist=as.numeric(rep(NA,length(type)))
                           )) %>% trans_df() 
  if(nrow(tmpResult2)==0) #empty window, return empty row 
  {return(resultEmpty)}
  
  if(method=="eventDist")
  {
    maxDistInData<-max(abs(tmpResult2$dist1))
    tmpResult3<-tmpResult2 %>% summarise(minDist1=min(abs(dist1),maxDist))
    tmpResult4<-left_join (levelTable,tmpResult3,by="type")
    if (maxDistInData>=maxDist)
    {
      tmpResult4<-tmpResult4 %>% 
        mutate(minDist1=replace(minDist1,is.na(minDist1),maxDist)) 
    }
    tmpResult4<-tmpResult4 %>% trans_df()
  }  
  if(method=="anyEvent")
  {
    tmpResult3<-tmpResult2 %>% filter(abs(dist1)<maxDist) %>% group_by(type)
    if (nrow(tmpResult3)==0) #empty window, return empty row 
    {return(resultEmpty)}
    tmpResult3 <- tmpResult3 %>% summarise(minDist1=n()>0)
    tmpResult4 <- left_join (levelTable,tmpResult3,by="type") %>%
      mutate(minDist1=!is.na(minDist1) & minDist1) %>% 
      trans_df()
  }
  tmpResult4
}
applyGetTypeDistances<-function(data,maxDist=24*3600,direction="forward",method="eventDist",append=TRUE)
{
  distTable<-ldply(1:nrow(data),getTypeDistances,data, maxDist=maxDist,direction=direction,method=method) 
  if(append){distTable<-cbind(data,distTable)}
  distTable %>% tbl_df()
}
refactor<-function(.data)
{colwise(function(col) {if (is.factor(col)) factor(col)  else col }) (.data)
}
trans_df<-function(.data,type=NA)
{
  tmpNames<-.data[,1]
  result <- as.data.frame(t(.data[,-1]),stringsAsFactors=FALSE)
  colnames(result) <- tmpNames
  if(!is.na(type)){result <- switch(type,character=result,
                                    factor=colwise(factor)(result),
                                    numeric=colwise(as.numeric)(result),
                                    logical=colwise(as.logical)(result))}
  result
}


#llply.cross.ab takes 2 lists la, lb, and a function .fun2
# for each element la1 in list la,
## for each element lb1 in list lb,
### perform a function .fun2 with la1, la2
### and store the results in a nested list
llply.cross.ab<-function(la,lb, .fun2=NULL, ...)
{
  llply(la,lb, .fun2, ... ,  .fun=function(la1,lb, .fun2, ...)
  {
    llply(lb,la1, .fun2, ..., .fun=function(lb1,la1, .fun2, ...)
    {
      if(!is.null(.fun2)){.fun2(la1,lb1, ...)}
    })
  })
}

#llply.2 takes a nested list l (2 levels), and a function .fun2
# for each element l1 in list l
## for each element l2 in list l1,
### perform a function .fun2 with l2
### and store the results in a nested list
llply.2<-function(l, .fun2=NULL, ...)
{
  llply(l,.fun2, ... , .fun=function(l1,.fun2, ...)
  {
    llply(l1,.fun2, ... , .fun=function(l2,.fun2, ...)
    {
      if(!is.null(.fun2)){.fun2(l2, ...)}
    })
  })
}

#llply.n takes a nested list l (n levels), a number of iterations, and a function .fun2
# for each element l1 in list l
## if n>1, call llply.n (l1,n-1,.fun2)
## otherwise, call .fun2(l1)
## and store the results in a nested list
llply.n<- function(l, n , .fun2=NULL, ...)
{
  llply(l, n ,.fun2, ..., .fun=function(l1,n, .fun2, ...)
  {
    if(n<=1){
      if(!is.null(.fun2)){.fun2(l1, ...)}
    }else
    {llply.n(l1,n-1,.fun2, ...)}
  }
  )
}

#unlist.n takes a nested list, and unlists the first n levels
unlist.n <- function(l,n)
{
  tmpList<-l
  for (i in 1:n){tmpList<-unlist(tmpList,recursive=FALSE)}
  tmpList
}


#llply.name performs the llply function over a list, but 
#passing the name of each element as an additional parameter
#
# Example: 'l' is a list with 2 elements: 'a' (num 1), and 'b' (num 2)
# llply.name(l,paste) returns a list with 2 elements: 'a' (char "1 a"), and 'b' (char "2 b")
llply.name <- function (l,.fun=NULL, ...)
{
  myfun<-.fun
  l1<- llply(names(l),l,myfun=myfun, ..., .fun=function(l.name,l,myfun, ...)
  {
    if(!is.null(myfun))
    {myfun(l[[l.name]], ...,l.name)}else
    {l[[l.name]]}
  })
  names(l1)<-names(l)
  l1
}

# llply.n.name is like llply.name, but applied over a nested list of 'n' levels
llply.n.name<- function(l, n , .fun=NULL, ...)
{
  myfun2<-.fun
  llply.name(l=l, n=n, myfun2=myfun2,  ..., .fun=function(l1,n, myfun2, ...)
  {
    if(n<=1){
      if(!is.null(myfun2)){myfun2(l1, ...)}
    }else
    {llply.n.name(l1,n-1,myfun2, ...)}
  }
  )
}

#llply.parallel.ab takes 2 nested lists la, lb, of level n, and a function .fun2
#
llply.parallel.ab <- function (l1, l2, n, .fun=NULL, ...)
{
  l1.labeled<-llply.n.labelname(l1,n,pasteLabels=FALSE)
  llply.n(l1.labeled,n,l2, .fun, .fun2=function(l11,l2,.fun)
  {
    l22<-get.nested(l2,attr(l11,".name"))
    if(!is.null(.fun)){.fun(l11,l22)} else{(l22)}
  })
}

#llply.parallel.multilist takes multiple nested lists in a list 'list.multi', a reference nested list 'list.ref',
# of level n, and a function .fun2. Using list.ref as index, it navigates through the nested lists 
# and applies .fun2 over groups of list elements
llply.parallel.multilist <- function (list.ref, list.multi, n, .fun=NULL, ...)
{
  list.ref<-llply.n.labelname(list.ref,n,pasteLabels=FALSE)
  
  llply.n(list.ref,n,list.multi, .fun, ... , .fun2=function(list.ref.1,list.multi,.fun, ...)
  {
    list.multi.1<-llply(list.multi, attr(list.ref.1,".name"), .fun=get.nested)
    if(!is.null(.fun)){.fun(list.multi.1, ...)} else{(list.multi.1)}
  })
}

# equivalent to l[[l.indices[[1]] ]] [[l.indices[[2]] ]] ... [[l.indices[[n]] ]]
get.nested<-function(l,l.indices)
{
  if(length(l.indices)>1)
  {get.nested(getElement(l,l.indices[[1]] ),l.indices[-1] ) } else
    getElement(l,l.indices[[1]])
}


#llply.labelname takes a list, and adds to each list element an attribute '.name' with the element name.

llply.labelname <-function(l, pasteLabels=TRUE)
{
  if(is.null(names(l))){return(l)}
  if(pasteLabels)
  {
    llply.name(l,.fun=function(ln,...){attr(ln,".name")<-paste(...,sep=".");ln})
  } else
  {
    llply.name(l,.fun=function(ln,...){attr(ln,".name")<-list(...) ;ln}) 
  }
}

# llply.n.labelname works like llply.labelname, but over a nested list of 'n' levels.
# WARNING: returns NULL if original list has no names
llply.n.labelname <-function(l,n, pasteLabels=TRUE)
{
  if(pasteLabels)
  {
    llply.n.name(l,n,.fun=function(ln,...){attr(ln,".name")<-paste(...,sep=".");ln})
  } else
  {
    llply.n.name(l,n,.fun=function(ln,...){attr(ln,".name")<-list(...) ;ln}) 
  }
}



