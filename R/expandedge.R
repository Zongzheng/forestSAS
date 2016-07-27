expandedge <-
function(X,xwidth,ywidth,id=1:X$n){
  if(xwidth>X$window$xrange[2]-X$window$xrange[1])
    stop("xwidth beyond the xrange")
  if(ywidth>X$window$yrange[2]-X$window$yrange[1])
    stop("ywidth beyond the yrange")
  totalX<-as.data.frame(cbind(x=X$x,y=X$y,X$marks,id))
  s.totalX<-totalX
  s.totalX$y<-s.totalX$y+ywidth
  uparea<-subset(totalX,y<=X$window$yrange[1]+ywidth)
  uparea$y<-uparea$y+(X$window$yrange[2]-X$window$yrange[1])
  downarea<-subset(totalX,y>=X$window$yrange[2]-ywidth)
  downarea$y<-downarea$y-(X$window$yrange[2]-X$window$yrange[1])
  updown_area<-rbind(uparea,totalX,downarea)
  s.updown_area<-updown_area
  rightarea<-subset(updown_area,x<=X$window$xrange[1]+xwidth)
  rightarea$x<-rightarea$x+(X$window$xrange[2]-X$window$xrange[1])
  leftarea<-subset(updown_area,x>=X$window$xrange[2]-xwidth)
  leftarea$x<-leftarea$x-(X$window$xrange[2]-X$window$xrange[1])
  leftright_area<-data.frame(rbind(leftarea,s.updown_area,rightarea))
  total_area<-as.data.frame(cbind(id=1:(nrow(leftarea)+nrow(rightarea)+nrow(updown_area)),old.id=leftright_area$id,leftright_area[-which(colnames(leftright_area)=="id")],row.names=NULL))
}
