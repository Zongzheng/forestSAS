storeydvd <-
function(height,minH=min(height),maxH=max(height),storeynum=6,
                 include.lowest = TRUE,right = TRUE){
  interval<-cut(height,breaks=seq(minH,maxH,length.out=storeynum+1),
                include.lowest = include.lowest,right = right)
  hdata<-data.frame(height,interval)
  hdata$storey<-paste("S",match(hdata$interval,
                                levels(interval)[length(levels(interval)):1]),sep="")
  list(heightfreq=table(interval),heightdata=hdata)
}


