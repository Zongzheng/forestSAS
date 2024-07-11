shrinkedge <-
function(X,xwidth,ywidth,id){
  stopifnot(spatstat.geom::is.ppp(X))
  x<-y<-NULL
  df.X<-as.data.frame(X)
  df.X.ppp<-spatstat.geom::ppp(x=df.X$x,y=df.X$y,
                          xrange=X$window$xrange,yrange=X$window$yrange)
  if(is.null(X$marks)){
    df.X.ppp<-df.X.ppp
  }else{
    spatstat.geom::marks(df.X.ppp)<-spatstat.geom::marks(df.X.ppp)
  }
  if(xwidth>(df.X.ppp$window$xrange[2]-df.X.ppp$window$xrange[1])/2)
    stop("xwidth beyond the xrange")
  if(ywidth>(df.X.ppp$window$yrange[2]-df.X.ppp$window$yrange[1])/2)
    stop("ywidth beyond the yrange")
  totalX<-as.data.frame(cbind(x=df.X.ppp$x,y=df.X.ppp$y,df.X.ppp$marks,id))
  yshrink<-subset(totalX,y>=(df.X.ppp$window$yrange[1]+ywidth)&y<=(df.X.ppp$window$yrange[2]-ywidth))
  xshrink<-subset(yshrink,x>=(df.X.ppp$window$xrange[1]+xwidth)&x<=(df.X.ppp$window$xrange[2]-xwidth))
  totaldata<-as.data.frame(cbind(id=1:nrow(xshrink),old.id=xshrink$id,xshrink[-which(colnames(xshrink)=="id")],row.names=NULL))
  return(totaldata)
}





