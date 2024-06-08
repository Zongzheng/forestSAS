nnid <-
function(X,N=NULL,R=NULL,id,exclude=TRUE){
  library(spatstat)
  dists <- spatstat.geom::applynbd(X, N=X$n,R=NULL,function(dists, ...){dists},exclude=FALSE)
  colnames(dists)=id
  addCol<-as.data.frame(cbind(T=1:X$n,dists))
  sortDists<-lapply(split(addCol[,-c(which(colnames(addCol)=="T"))],list(addCol$T)),function(x) structure(sort(as.numeric(x)), names = names(x)[order(as.numeric(x))]))
  excludeid<-lapply(sortDists,function(x) x[-1])
  if(!is.null(N)){
    extractName<-list_to_matrix(lapply(excludeid,names))[,1:N]
    nnid<-cbind(id=id,data.frame(extractName))
  }
  if(!is.null(R)){
    minnndist=spatstat.geom::minnndist(X=X)
    if(R<=spatstat.geom::minnndist(X=X))
      stop(paste("R must exceed the minimum nearest-neighbour distance (",minnndist,")",sep=""))
    selectR<-lapply(excludeid,function(x) x[which(x<=R)])
    extractName<-list_to_matrix(lapply(selectR,names))
    if(nrow(extractName)==1){
      nnid<-cbind(id=id,data.frame(t(extractName)))
    }else{
      nnid<-cbind(id=id,data.frame(extractName))
    }
  }
  names(nnid)<-c("id",paste("id",1:(ncol(nnid)-1),sep=""))
  if(exclude){
    nnid<-nnid[,which(colnames(nnid)!="id"),drop=FALSE]
  }else{
    nnid<-nnid
  }
  return(nnid)
}
