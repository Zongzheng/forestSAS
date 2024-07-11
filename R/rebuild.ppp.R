rebuild.ppp<-function(X,id=1:(X$n),rm.id=NULL,add.X=NULL,add.id=paste("add",1:(add.X$n),sep="")){
  stopifnot(spatstat.geom::is.ppp(X))
  df.X<-as.data.frame(X)
  df.X.ppp<-spatstat.geom::ppp(x=df.X$x,y=df.X$y,
                      xrange=X$window$xrange,yrange=X$window$yrange)
  if(is.null(X$marks)){
    spatstat.geom::marks(df.X.ppp)=id
  }else{
    spatstat.geom::marks(df.X.ppp)=data.frame(id=id,X$marks)
  }
  newX=df.X.ppp
  if (!is.null(rm.id)){
    if(any(is.na(match(rm.id,id))))
      stop("rm.id should be included in the id")
    newX<-df.X.ppp[-match(rm.id,id),]
  }
  if (!is.null(add.X)){
    if(any(intersect(add.id,id)))
      stop("add.id must different with id")
    if(is.null(add.X$marks)){
      spatstat.geom::marks(add.X)=add.id
    }else{
      spatstat.geom::marks(add.X)=data.frame(id=add.id,add.X$marks)
    }
    newX<-spatstat.geom::superimpose(df.X.ppp,add.X)
  }
  return(newX)
}
