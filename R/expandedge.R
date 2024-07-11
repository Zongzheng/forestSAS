expandedge<-
  function (X, xwidth, ywidth,id = 1:X$n, marks=X$marks,type="com"){
    stopifnot(spatstat.geom::is.ppp(X))
    Type <- c("com", "ppp")
    type <- match.arg(type, Type)
    x <- y <- NULL
    df.X <- as.data.frame(X)
    df.X.ppp <- spatstat.geom::ppp(x = df.X$x, y = df.X$y, xrange = X$window$xrange,
                              yrange = X$window$yrange)
    if (is.null(X$marks)) {
      df.X.ppp <- df.X.ppp
    }
    else {
      spatstat.geom::marks(df.X.ppp) <- spatstat.geom::marks(X)
    }
    if (xwidth > df.X.ppp$window$xrange[2] - df.X.ppp$window$xrange[1])
      stop("xwidth beyond the xrange")
    if (ywidth > df.X.ppp$window$yrange[2] - df.X.ppp$window$yrange[1])
      stop("ywidth beyond the yrange")
    totalX <- as.data.frame(cbind(x = df.X.ppp$x, y = df.X.ppp$y,
                                  df.X.ppp$marks, id))
    s.totalX <- totalX
    s.totalX$y <- s.totalX$y + ywidth
    uparea <- subset(totalX, y <= df.X.ppp$window$yrange[1] +
                       ywidth)
    uparea$y <- uparea$y + (df.X.ppp$window$yrange[2] - df.X.ppp$window$yrange[1])
    downarea <- subset(totalX, y >= df.X.ppp$window$yrange[2] -
                         ywidth)
    downarea$y <- downarea$y - (df.X.ppp$window$yrange[2] - df.X.ppp$window$yrange[1])
    updown_area <- rbind(uparea, totalX, downarea)
    s.updown_area <- updown_area
    rightarea <- subset(updown_area, x <= df.X.ppp$window$xrange[1] +
                          xwidth)
    rightarea$x <- rightarea$x + (df.X.ppp$window$xrange[2] -
                                    df.X.ppp$window$xrange[1])
    leftarea <- subset(updown_area, x >= df.X.ppp$window$xrange[2] -
                         xwidth)
    leftarea$x <- leftarea$x - (df.X.ppp$window$xrange[2] - df.X.ppp$window$xrange[1])
    leftright_area <- data.frame(rbind(leftarea, s.updown_area,
                                       rightarea))
    total_area <- as.data.frame(cbind(id = 1:(nrow(leftarea) +
                                                nrow(rightarea) + nrow(updown_area)), old.id = leftright_area$id,
                                      leftright_area[-which(colnames(leftright_area) == "id")],
                                      row.names = NULL))
    total_area<-total_area[c(names(total_area)[c(3:4)],names(total_area)[-c(3:4)])]
    if(type=="com"){
      total= total_area
    }else if(type=="ppp"){
      total=spatstat.geom::ppp(x= total_area$x,y= total_area$y,
                               xrange=c(X$window$xrange[1]-xwidth,
                                        X$window$xrange[2]+xwidth),
                               yrange=c(X$window$yrange[1]-ywidth,
                                        X$window$yrange[2]+ywidth),
                         marks=total_area[-c(1:2)])
    }
    return(total)
  }









