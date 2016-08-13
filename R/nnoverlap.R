nnoverlap <-
function(nncrown,nndist){
  nncrown.half<-nncrown/2
  sum.crown<-as.data.frame(t(apply(nncrown.half,1,
                                   function(x)cbind(x[2:5]+x[1]))))
  overlap<-sum.crown-nndist
  names(overlap)<-paste("overlap",1:ncol(overlap),sep="")
  return(overlap)
}
