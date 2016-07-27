list_to_matrix<-function(x,item=NA){
  maxlength<-max(sapply(x,length))
  return(t(sapply(x,function(row,maxlength) c(row,rep(item,maxlength-length(row))),maxlength)))
}