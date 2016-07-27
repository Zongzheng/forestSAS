rebild.ppp<-function(X,id=1:(X$n),rm.id=NULL,add.id=NULL,add.x=NULL,add.y=NULL,add.mark=NULL){
  data<-as.data.frame(X)
  data$id<-id
  if (!is.null(rm.id)){
    cat("Warning:rm.id should be included in the id")
    newX<-data[-match(rm.id,id),]
    newX.ppp<-ppp(x=newX$x,y=newX$y,X$window$xrange,X$window$yrange,
                  marks=newX[,-c(1:2)])
  }
  else if (!is.null(add.id)){
    if(any(intersect(add.id,id)))
      stop("add.id must different with id")
    if(!is.null(add.mark)){
      cat("Warning:please ensure same variables in add.mark must with same colname of X$mark")
      if (!all(length(add.id)==length(add.x),length(add.id)==length(add.y),
               length(add.id)==nrow(add.mark)))
        stop("add.id,add.x,add.y,and nrow of add.mark must with same length")
      add.data<-as.data.frame(cbind(x=add.x,y=add.y,add.mark))
    }else{
      if (!all(length(add.id)==length(add.x),length(add.id)==length(add.y)))
        stop("add.id,add.x,and add.y must with same length")
      add.data<-as.data.frame(cbind(x=add.x,y=add.y))
    }
    add.data$id<-add.id
    newX=merge(add.data,data,all=TRUE)
    newX.ppp<-ppp(x=newX$x,y=newX$y,X$window$xrange,X$window$yrange,
                  marks=newX[,-c(1:2)])
  }else{newX.ppp=ppp(x=data$x,y=data$y,X$window$xrange,X$window$yrange,
                     marks=data[,-c(1:2)])}
  return(newX.ppp)
}
