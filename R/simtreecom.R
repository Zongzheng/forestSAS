simtreecom<-
  function (size = 10, nspe = 10, nspe_max = 36, xrange = c(0,
                                                            100), yrange = c(0, 100), dbhrange = c(5, 50), type = "com",lambda=1)
  {
    if (nspe > nspe_max) {
      stop(paste("Maximum number of species is", nspe_max))
    }
    Type <- c("com", "ppp")
    type <- match.arg(type, Type)
    samspe <- function(size, nspe) {
      spenchar = paste("S", 1:nspe, sep = "")
      if (size == nspe) {
        res <- sample(x = spenchar, size = size, replace = FALSE)
      }
      else if (size < nspe) {
        stop("sample size must be greater than number of species")
      }
      else if (size > nspe) {
        res <- c(sample(x = spenchar, size = nspe, replace = FALSE),
                 sample(x = spenchar, size = size - nspe, replace = TRUE))
      }
      return(res)
    }
    transformData <- function(data, minRange, maxRange) {
      transformedData <- (data - min(data)) * (maxRange - minRange) / (max(data) - min(data)) + minRange
      return(transformedData)
    }
    f<- function(x,y,lambda) {exp(lambda*x)}
    points <- as.data.frame(spatstat.random::rpoint(n = size,f,lambda=lambda))
    points$x=transformData(points$x, xrange[1]+(xrange[2]-xrange[1])*0.00001,
                           xrange[2]-(xrange[2]-xrange[1])*0.00001)
    points$y=transformData(points$y, yrange[1]+(yrange[2]-yrange[1])*0.00001,
                           yrange[2]-(yrange[2]-yrange[1])*0.00001)
    points$id <- 1:size
    points$spe <- samspe(size = size, nspe = nspe)
    points$dbh <- sample(x = seq(dbhrange[1], dbhrange[2], 0.001),
                         size = size, replace = TRUE)
    points$ht <- round(1.3 + 31.462 * (1 - exp(-0.04 * points$dbh))^1.362,
                       3)
    points$cw <- round(0.529 * points$dbh^0.6749, 3)
    points$hcb <- round(points$ht/(1 + exp(0.0133 * points$dbh)),
                        3)
    points$volume = round((points$ht + 3) * 0.42 * pi * ((points$dbh/2)^2)/10000,
                          4)
    quality_class <- c("good", "average", "poor")
    points$quality <- c(sample(x = quality_class, size = 3, replace = FALSE),
                        sample(x = quality_class, size = size - 3, replace = TRUE))
    if (type == "com") {
      simcom = points
    }
    else if (type == "ppp") {
      simcom = spatstat.geom::ppp(x = points$x, y = points$y,
                                  xrange = xrange, yrange = yrange, marks = points[3:10])
    }
    return(simcom)
  }
