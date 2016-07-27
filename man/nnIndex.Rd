\name{nnIndex}
\alias{nnIndex}
\title{
Marks of nearest neighbour
}
\description{
Get the marks of nearest neighbour in the point pattern
}
\usage{
nnIndex(X, id = 1:(X$n), smark = NULL, N = NULL, R = NULL, rm.id = NULL, add.id = NULL, add.x = NULL, add.y = NULL, add.mark = NULL, cal.buf = TRUE, buffer = FALSE, buf.xwid = 5, buf.ywid = 5, exclude = TRUE)
}
\arguments{
  \item{X}{
A marked point pattern (object of class "ppp").
}
  \item{id}{
Specific identification number of each row in the point pattern.
}
  \item{smark}{
Selected marks to find the marks of nearest neighbour.
}
  \item{N}{
Specific number of points in the neighborhood, the value is always assinged 4.
}
  \item{R}{
Specific circular neighborhood, the value is always assinged 5.
}
  \item{rm.id}{
Needed removed the specific rows in the point pattern, refer to the id.
}
  \item{add.id}{
Needed added the specific rows in the point pattern.
}
  \item{add.x}{
Vector of x coordinates of data points for add.id.
}
  \item{add.y}{
Vector of y coordinates of data points for add.id.
}
  \item{add.mark}{
Mark values for the points for add.id. A vector or data frame.
}
  \item{cal.buf}{
Wether identifing the buffer zone,if cal.buf is TRUE,need identifying the buffer zone,if
if cal.buf is FALSE,need not identifying the buffer zone.
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
  \item{exclude}{
Wether including the data of the buffer zone,
if exclude is TRUE the result return the data remove the data of the buffer zone,
and only the data of the core zone;if exclude is FALSE the result return the data with buffer and core zone.
}
}
\details{
Given a marked point pattern dataset X this function computes, for each desired location y, the mark attached to the point of X that is nearest to y. The desired locations y can be either a pixel grid or the point pattern X itself.see also the function "nnmark" in the
package spatstat
}
\value{
multiple "nnmarks" are the selected marks (smark) of nearest neighbour
nnid is the id of nearest neighbour
nndist is the distance of nearest neighbour
data is the final point pattern
}
\author{
Zongzheng Chai, chaizz@126.com
}
\seealso{
the function "nnmark" in the package spatstat
}
\examples{
#### Based on specific number (N=4) of nearest neighbour####
## Basic usage
nndN<-nnIndex(finpines,N=4,smark="diameter",buffer=TRUE,buf.xwid =2,buf.ywid=3)
nndN
nndhN<-nnIndex(finpines,N=4,smark=c("diameter","height"),buffer=TRUE,buf.xwid =2,buf.ywid=3)
nndN

# Only the points in the core zone (Removed the points in the buffer zone)
nndN_core<-nnIndex(finpines,N=4,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndN_core
nndhN_core<-nnIndex(finpines,N=4,smark=c("diameter","height"),buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndN_core

## Remove some points, and then identify the marks  of Nearest Neighbour
nndN.rm_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),
              rm.id=c("T1","T3","T8","T9","T59","T60","T120"),
              N=4,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndN.rm_core
## add some points, and then identify the marks  of Nearest Neighbour
marks=data.frame(diameter=c(2.0,3.0,4.0,5.0,6.0),
                 height=c(2.5,3.5,4.5,5.5,6.5))
nndN.add_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),
                       add.x=c(-2,-1,0,2,4),add.y=c(-4,-3,-6,0,1),
                       add.id=paste("NT",1:5,sep=""),
                       add.mark=marks,
                       N=4,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndN.add_core


#### Based on specific circular (R=5) of nearest neighbour####
## Basic usage
nndR<-nnIndex(finpines,R=5,smark="diameter",buffer=TRUE,buf.xwid =2,buf.ywid=3)
nndR
nndhR<-nnIndex(finpines,R=5,smark=c("diameter","height"),buffer=TRUE,
        buf.xwid =2,buf.ywid=3)
nndhR

# Only the points in the core zone (Removed the points in the buffer zone)
nndR_core<-nnIndex(finpines,R=5,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndR_core
nndhR_core<-nnIndex(finpines,R=5,smark=c("diameter","height"),buffer=FALSE,
            buf.xwid =2,buf.ywid=3)
nndhR_core

## Remove some points, and then identify the marks  of Nearest Neighbour
nndR.rm_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),
                        rm.id=c("T1","T3","T8","T9","T59","T60","T120"),
                        R=5,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndR.rm_core

## add some points, and then identify the marks  of Nearest Neighbour
marks=data.frame(diameter=c(2.0,3.0,4.0,5.0,6.0),
                 height=c(2.5,3.5,4.5,5.5,6.5))
nndR.add_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),
                       add.x=c(-2,-1,0,2,4),add.y=c(-4,-3,-6,0,1),
                       add.id=paste("NT",1:5,sep=""),
                       add.mark=marks,
                       R=5,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndR.add_core
}
