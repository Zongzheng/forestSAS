\name{spastr}
\alias{spastr}
\title{
Computing forest spatial structure indices.
}
\description{
Computing forest spatial structure indices.
}
\usage{
spastr(X_df,smark=c("spe","storey","dbh","cw","x","y"),
                 rm.id = NULL,
                 buffer = FALSE,
                 xrange=c(0,100),yrange=c(0,100),
                 xwidth=5,ywidth=5,
                 buf.xwid = 5, buf.ywid = 5,
                 exclusion = FALSE)
}
\arguments{
  \item{X_df}{
Forest community data (object of class "data.frame")
}
  \item{smark}{
Selected marks to compute forest spatial structure indices.
}
  \item{rm.id}{
Needed removed the specific rows in the point pattern, refer to the id.
}
  \item{buffer}{
if buffer is TRUE,show the all of data with buffer and core zone.
if buffer is FALSE,only show the data in the core zone.
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
  \item{exclusion}{
Wether including the data of the buffer zone,
if exclude is TRUE the result return the data remove the data of the buffer zone,
and only the data of the core zone;if exclude is FALSE the result return the data with buffer and core zone.
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
treecom_spastr<-spastr(X_df=treecom_example,
              xrange=c(0,100),yrange=c(0,100),
              xwidth=10,ywidth=10,
              buf.xwid =10, buf.ywid = 10,
              smark=c("spe","storey","dbh","cw","x","y"))
treecom_spastr
}
