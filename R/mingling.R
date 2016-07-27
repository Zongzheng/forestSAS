mingling <-
function(X,id=1:(X$n),smark=NULL,
                   rm.id=NULL,add.id=NULL,add.x=NULL,add.y=NULL,add.mark=NULL,
                   cal.buf=TRUE,buffer=FALSE,buf.xwid=5,buf.ywid=5,exclude=TRUE){
  nnindex<-nnIndex(X=X,id=id,smark=smark,N=4,
                   rm.id=rm.id,add.id=add.id,add.x=add.x,add.y=add.y,add.mark=add.mark,
                   cal.buf=cal.buf,buffer=buffer,buf.xwid=buf.xwid,buf.ywid=buf.ywid,exclude=exclude)
  nnattri<-paste("nn",smark,sep="")
  nnsp<-nnindex[[nnattri]]
  chooseDiffer<-function(x){
    k<-length(which(x[2:5]!=x[1]))
    if(k==0){m=0}
    if(k==1){m=0.25}
    if(k==2){m=0.5}
    if(k==3){m=0.75}
    if(k==4){m=1}
    return(m)
  }
  M<-apply(nnsp,1,chooseDiffer)
  M<-factor(M,levels=c(0,0.25,0.5,0.75,1))
  Mcount<-table(M)
  Mfreq<- Mcount/sum(Mcount)
  meanM<-sum(t(Mcount)*as.numeric(names(Mcount)))/sum(Mcount)
  list(result=cbind(nnsp,M),Mcount=Mcount,
       Mfreq=Mfreq,meanM=meanM,data=nnindex$data)
}
