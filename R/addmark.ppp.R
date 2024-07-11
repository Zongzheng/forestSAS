addmark.ppp <-
function(X,add.mark,add.name="storey"){
  stopifnot(spatstat.geom::is.ppp(X))
  df.X<-as.data.frame(X)
  df.addX<-data.frame(cbind(df.X,add.mark))
  names(df.addX)<-c(names(df.X),add.name)
  newX<-spatstat.geom::ppp(X$x,X$y,window=X$window,
     marks=df.addX[,-match(c("x","y"),colnames(df.addX))])
  return(newX)
}
