nnIndex <-
function(X,id=1:(X$n),smark=NULL,N=NULL,R=NULL,
        rm.id=NULL,add.id=NULL,add.x=NULL,add.y=NULL,add.mark=NULL,
        cal.buf=TRUE,buffer=FALSE,buf.xwid=5,buf.ywid=5,exclude=TRUE){
  ppp=rebild.ppp(X=X,id=id,rm.id=rm.id,add.id=add.id,add.x=add.x,add.y=add.y,add.mark=add.mark)
  if(cal.buf){
    buf.ppp<-buffer(X=ppp,buf.xwid=buf.xwid,buf.ywid=buf.ywid)
  }else{
    buf.ppp<-ppp}
  data<-as.data.frame(buf.ppp)
  if (!is.null(N)){
    nndist <- t(applynbd(X=ppp,N=N,R=R,function(dists, ...){dists},exclude=exclude))
    nnid<-t(applynbd(X=ppp,N=N,R=R,function(Y, ...) {Y$marks$id},exclude=exclude))
  }
  if(!is.null(R)){
    nndist <- applynbd(X=ppp,N=N,R=R,function(dists, ...){dists},exclude=exclude)
    nndist<-list_to_matrix(nndist)
    nnid<-applynbd(X=ppp,N=N,R=R,function(Y, ...) {Y$marks$id},exclude=exclude)
    nnid<-list_to_matrix(nnid)
  }
    nndist<-as.data.frame(nndist)
    nnid<-as.data.frame(nnid)
    colnames(nndist)=paste("dist",1:ncol(nndist),sep="")
    colnames(nnid)=paste("id",1:ncol(nnid),sep="")
    nnid<-cbind(id=data$id,nnid)
  C<-list()
  H<-list()
  for (i in 1:length(smark)){
    for(m in 1:ncol(nnid)){
      C[m][[1]]<-data[smark[i]][,1][match(nnid[,m],data$id)]
    }
    H[[i]]<-sapply(C,as.matrix)
  }
  nnIndex<-lapply(H,as.data.frame)
  names(nnIndex)<-paste("nn",smark,sep="")
  for (i in 1:length(nnIndex)){
    names(nnIndex[[i]])=c(smark[i],paste(smark[i],1:(length(names(nnIndex[[i]]))-1),sep=""))
  }
  nnIndex$nnid=nnid
  nnIndex$nndist=nndist
  if(buffer){
    nnIndex<-nnIndex
  }else{
    nnIndex<-lapply(nnIndex,function(x) x[-which(data$zone=="buffer"),])
  }
    nnIndex$data<-buf.ppp
    return(nnIndex)
}
