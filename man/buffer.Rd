\name{buffer}
\alias{buffer}
\title{
Identify the buffer zone in the point pattern
}
\description{
To eliminate edge effects and improve the accuracy of the
Calculation,such as analyzing the forest spatial structure
parameters, we always established a buffer
zone around the plot. In the statistical analysis, only the
trees in the reduced window are used as
reference trees, and the individual trees in the buffer zone
are only considered to be the nearest neighbors of the
trees in the reduced window. This edge
correction can individually evaluate each tree to determine
whether all n nearest neighbors are truly located within
the plot.
}
\usage{
buffer(X, buf.xwid = 5, buf.ywid = 5)
}
\arguments{
  \item{X}{
A point pattern (object of class "ppp").
}
  \item{buf.xwid}{
The width of buffer zone in the x coordinates of data points
}
  \item{buf.ywid}{
The width of buffer zone in the y coordinates of data points
}
}
\value{
An object of class "ppp", which add a marks "zone" in the orginal point pattern
}
\references{
Chai ZZ, Sun CL, Wang DX, Liu WZ, and Zhang CS.2016.
Spatial structure and dynamics of predominant populations in a virgin
old-growthoak forest in the Qinling Mountains, China. Scandinavian Journal of Forest Research. DOI:
10.1080/02827581.2016.1183703
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
library(spatstat)
####Based on the simulated data####
# Creating a simulated point pattern
x <- runif(20)
y <- runif(20)
X <- ppp(x, y, c(0,1), c(0,1))
X
# Identifing the buffer zone and core zone in the point pattern
# The width of buffer zone in the x coordinates of data points is 0.2
# The width of buffer zone in the y coordinates of data points is 0.3
X_buf<-buffer(X,buf.xwid =0.2,buf.ywid=0.3)
X_buf
as.data.frame(X_buf)

####Based on the example data####
data(finpines)
finpines
# Marked planar point pattern: 126 points
# Mark variables: diameter, height
# window: rectangle = [-5, 5] x [-8, 2] metres

# Identifing the buffer zone and core zone in the finpines
# The width of buffer zone in the x coordinates of finpines is 2
# The width of buffer zone in the y coordinates of finpines is 3
finpines_buf<-buffer(finpines,buf.xwid =2,buf.ywid=3)
finpines_buf
as.data.frame(finpines_buf)
}
