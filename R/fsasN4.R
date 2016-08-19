fsasN4 <-
function(nnattri,match.fun,para=NULL){
  if(is.null(para)){
    Index<-apply(nnattri,1,match.fun)
  }else{
    Index<-apply(nnattri,1,match.fun,para=para)
  }
    Index<-I
    Index<-as.numeric(as.character(Index))
    I<-factor(I,levels=c(0,0.25,0.5,0.75,1))
    Icount<-table(I)
    Ifreq<- Icount/sum(Icount)
    meanI<-sum(t(Icount)*as.numeric(names(Icount)))/sum(Icount)
    list(result=cbind(nnattri,Index),Icount=Icount,
         Ifreq=Ifreq,meanI=meanI)
  }
