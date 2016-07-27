TSS <-
function(nnsp){
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
  S<-apply(nnsp,1,function(x) c(length(unique(x))))
  MS<-M*S/nrow(nnsp)
  M<-factor(M,levels=c(0,0.25,0.5,0.75,1))
  Mcount<-table(M)
  Mfreq<- Mcount/sum(Mcount)
  meanM<-sum(t(Mcount)*as.numeric(names(Mcount)))/sum(Mcount)
  list(result=cbind(nnsp,M,S,MS),Mcount=Mcount,
       Mfreq=Mfreq,meanM=meanM,TSS=sum(MS))
}
