fsasN4 <-
function(nnattri,match.fun,para=NULL){
  if(is.null(para)){
    ind<-apply(nnattri,1,match.fun)
  }else{
    ind<-apply(nnattri,1,match.fun,para=para)
  }
    Ind<-ind
    Ind<-factor(Ind,levels=c(0,0.25,0.5,0.75,1))
    Icount<-table(Ind)
    Ifreq<- Icount/sum(Icount)
    meanI<-sum(t(Icount)*as.numeric(names(Icount)))/sum(Icount)
    list(result=cbind(nnattri,index=ind),Icount=Icount,
         Ifreq=Ifreq,meanI=meanI)
  }
