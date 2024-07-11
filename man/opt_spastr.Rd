\name{opt_spastr}
\alias{opt_spastr}
\title{
Optimal forest spatial structure indices.
}
\description{
Optimal forest spatial structure indices.
}
\usage{
opt_spastr(X_df,inter=100,pop=0.2,
                 smark=c("spe","storey","dbh","cw","x","y"),
                 xrange=c(0,100),yrange=c(0,100),
                 xwidth=5,ywidth=5,
                 buf.xwid = 5, buf.ywid = 5)
}
\arguments{
  \item{X_df}{
Forest community data (object of class "data.frame")
}
  \item{inter}{
Inter quanitity.
}
  \item{pop}{
Cutting intensity.
}
  \item{smark}{
Selected marks to compute forest spatial structure indices.
}
  \item{rm.id}{
Needed removed the specific rows in the point pattern, refer to the id.
}
  \item{buf.xwid}{
The width of buffer zone in the x coordinates of data points.
}
  \item{buf.ywid}{
The width of buffer zone in the y coordinates of data points.
}
  \item{xrange}{
Range of X axes
}
  \item{yrange}{
Range of Y axes
}
  \item{xwidth}{
Length of buffer zone of X axes
}
  \item{ywidth}{
Length of buffer zone of Y axes
}
}
\details{
NULL
}
\value{
NULL
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
data(treecom_example)
head(treecom_example)
treecom_opt<-opt_spastr(X_df=treecom_example,inter=100,pop=0.2,
              xrange=c(0,100),yrange=c(0,100),
              xwidth=10,ywidth=10,
              buf.xwid =10, buf.ywid = 10,
              smark=c("spe","storey","dbh","cw","x","y"))
treecom_opt
}
