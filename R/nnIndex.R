nnIndex<-function(X,id=1:(X$n),smark=NULL,N=NULL,R=NULL,
                  rm.id=NULL,add.X=NULL,add.id=paste("add",1:(add.X$n),sep=""),
                  buffer=FALSE,buf.xwid=5,buf.ywid=5,exclusion=FALSE){
  ppp=rebild.ppp(X=X,id=id,rm.id=rm.id,add.X=add.X,add.id=add.id)
  buf.ppp<-buffer(X=ppp,buf.xwid=buf.xwid,buf.ywid=buf.ywid)
  zone=as.data.frame(buf.ppp)[c("id","x","y","zone")]
  data<-as.data.frame(buf.ppp)
  loca<-data.frame(X$x,X$y)
  d<-as.data.frame(as.matrix(dist(loca,"euclidean")))
  colnames(d)=id
  data1<-as.data.frame(cbind(id=id,d))
  data2<-lapply(split(data1[,-c(which(colnames(data1)=="id"))],list(data1$id)),sort)
  data3<-lapply(data2,function(x) x[-1])
  if (!is.null(N)){
    nndist <- applynbd(X=ppp,N=N,function(dists, ...){sort(dists)},exclude=TRUE)
    data4<-list_to_matrix(lapply(data3,names))[,1:N]
    nnid<-t(matrix(as.numeric(as.character(data4)),nrow=nrow(data)))
    if(N==1){
      nndist<-as.data.frame(nndist)
      nnid<-as.data.frame(nnid)
    }else{
      nndist<-as.data.frame(t(nndist))
      nnid<-as.data.frame(t(nnid))}
  }
  if(!is.null(R)){
    minnndist=minnndist(X=ppp)
    if(R<=minnndist(X=ppp))
      stop(paste("R must exceed the minimum nearest-neighbour distance (",minnndist,")",sep=""))
    nndist <- applynbd(X=ppp,R=R,function(dists, ...){sort(dists)},exclude=TRUE)
    nndist<-list_to_matrix(nndist)
    data3a<-lapply(data3,function(x,n) x[which(x<=n)],n=R)
    data4a<-list_to_matrix(lapply(data3a,names))
    nnid<-matrix(as.numeric(as.character(data4)),nrow=nrow(data))
    if(nrow(nndist)==1){
      nndist<-as.data.frame(t(nndist))
      nnid<-as.data.frame(t(nnid))
    }else{
      nndist<-as.data.frame(nndist)
      nnid<-as.data.frame(nnid)}
  }
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
  nnIndex$nndist=nndist
  nnIndex$nnid=nnid
  if(exclusion){
    nnIndex<-lapply(nnIndex,function(x) x[,-1])
  }else{nnIndex<-nnIndex}
  nnIndex$zone=zone
  if(buffer){
    nnIndex<-nnIndex
  }else{
    nnIndex<-lapply(nnIndex,function(x) as.data.frame(x)[-which(data$zone=="buffer"),])
  }
  nnIndex$data<-buf.ppp
  return(nnIndex)
}




























































