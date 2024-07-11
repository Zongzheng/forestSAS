buffer <-
function(X,buf.xwid=5,buf.ywid=5){
  stopifnot(spatstat.geom::is.ppp(X))
  data<-as.data.frame(X)
  range<-subset(data,data$x>=(X$window$xrange[1]+buf.xwid)&data$x<=(X$window$xrange[2]-buf.xwid)&
                  data$y>=(X$window$yrange[1]+buf.ywid)&data$y<=(X$window$yrange[2]-buf.ywid))
  data$zone<-"buffer"
  data$zone[match(row.names(range),row.names(data),)]<-"core"
  newX<-spatstat.geom::ppp(x=data$x,y=data$y,X$window$xrange,X$window$yrange,
            marks=data[,-c(1:2)])
  return(newX)
}
