rebuild.ppp<-function(X,id=1:(X$n),rm.id=NULL,add.X=NULL,add.id=paste("add",1:(add.X$n),sep="")){
  if(is.null(X$marks)){
    marks(X)=id
  }else{
    marks(X)=data.frame(id=id,X$marks)
  }
  newX=X
  if (!is.null(rm.id)){
    if(any(is.na(match(rm.id,id))))
      stop("rm.id should be included in the id")
    newX<-X[-match(rm.id,id),]
  }
  if (!is.null(add.X)){
    if(any(intersect(add.id,id)))
      stop("add.id must different with id")
    if(is.null(add.X$marks)){
      marks(add.X)=add.id
    }else{
      marks(add.X)=data.frame(id=add.id,add.X$marks)
    }
    newX<-superimpose(X,add.X)
  } 
  return(newX)
}