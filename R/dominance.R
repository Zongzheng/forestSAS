dominance <-
function(X,id=1:(X$n),smark=NULL,
                    rm.id=NULL,add.id=NULL,add.x=NULL,add.y=NULL,add.mark=NULL,
                    cal.buf=TRUE,buffer=FALSE,buf.xwid=5,buf.ywid=5,exclude=TRUE){
  nnindex<-nnIndex(X=X,id=id,smark=smark,N=4,
                   rm.id=rm.id,add.id=add.id,add.x=add.x,add.y=add.y,add.mark=add.mark,
                   cal.buf=cal.buf,buffer=buffer,buf.xwid=buf.xwid,buf.ywid=buf.ywid,exclude=exclude)
  nnattri<-paste("nn",smark,sep="")
  nnattribute<-nnindex[[nnattri]]
  comparson<-function(x){
    k<-length(which(x[2:5]>x[1]))
    if(k==0){u=0}
    if(k==1){u=0.25}
    if(k==2){u=0.5}
    if(k==3){u=0.75}
    if(k==4){u=1}
    return(u)
  }
  U<-apply(nnattribute,1,comparson)
  U<-factor(U,levels=c(0,0.25,0.5,0.75,1))
  Ucount<-table(U)
  Ufreq<- Ucount/sum(Ucount)
  meanU<-sum(t(Ucount)*as.numeric(names(Ucount)))/sum(Ucount)
  list(result=cbind(nnattri,U),Ucount=Ucount,
       Ufreq=Ufreq,meanU=meanU)
}
