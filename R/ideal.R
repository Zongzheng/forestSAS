ideal <-
function(x,para){
    k<-length(which(!is.na(match(x[2:5],para))))
    if(k==0){m=0}
    if(k==1){m=0.25}
    if(k==2){m=0.5}
    if(k==3){m=0.75}
    if(k==4){m=1}
    return(m)
}
