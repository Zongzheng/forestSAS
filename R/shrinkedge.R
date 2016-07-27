shrinkedge <-
function(X,xwidth,ywidth,id){
  if(xwidth>(X$window$xrange[2]-X$window$xrange[1])/2)
    stop("xwidth beyond the xrange")
  if(ywidth>(X$window$yrange[2]-X$window$yrange[1])/2)
    stop("ywidth beyond the yrange")
  totalX<-as.data.frame(cbind(x=X$x,y=X$y,X$marks,id))
  yshrink<-subset(totalX,y>=(X$window$yrange[1]+ywidth)&y<=(X$window$yrange[2]-ywidth))
  xshrink<-subset(yshrink,x>=(X$window$xrange[1]+xwidth)&x<=(X$window$xrange[2]-xwidth))
  totaldata<-as.data.frame(cbind(id=1:nrow(xshrink),old.id=xshrink$id,xshrink[-which(colnames(xshrink)=="id")],row.names=NULL))
  return(totaldata)
}
