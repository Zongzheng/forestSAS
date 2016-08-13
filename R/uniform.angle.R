uniform.angle <-
function(x,para){
  k<-length(which(x[1:4]>=para))
  if(k==0){m=1}
  if(k==1){m=0.75}
  if(k==2){m=0.5}
  if(k==3){m=0.25}
  if(k==4){m=0}
  return(m)
}
