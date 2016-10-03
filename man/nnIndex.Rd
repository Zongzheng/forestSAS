\name{nnIndex}
\alias{nnIndex}
\title{
Get the marks of nearest neighbour in the point pattern
}
\description{
Get the marks of nearest neighbour in the point pattern
}
\usage{
nnIndex(X, id = 1:(X$n), smark = NULL, N = NULL,
        R = NULL, rm.id = NULL, add.X = NULL,
        add.id = paste("add", 1:(add.X$n), sep = ""),
        buffer = FALSE, buf.xwid = 5, buf.ywid = 5,
        exclusion = FALSE)
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
  \item{add.X}{
Another point pattern need added in the point pattern.
}
  \item{add.id}{
Specific identification number of each row in the added point pattern.
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
  \item{exclusion}{
Wether including the data of the buffer zone,
if exclude is TRUE the result return the data remove the data of the buffer zone,
and only the data of the core zone;if exclude is FALSE the result return the data with buffer and core zone.
}
}
\details{
Given a marked point pattern dataset X this function computes, for each desired location y, the mark attached to the point of X that is nearest to y. The desired locations y can be either a pixel grid or the point pattern X itself.see also the function "nnmark" in the package spatstat
}
\value{
Multiple "nnmarks" are the selected marks (smark) of nearest neighbour
nnid is the id of nearest neighbour
nndist is the distance of nearest neighbour
data is the final point pattern
}
\author{
Zongzheng Chai, chaizz@126.com
}
\seealso{
The function "nnmark" in the package spatstat
}
\examples{
library(spatstat)
data(finpines)
#### Based on specific number (N=4) of nearest neighbour####
## Basic usage
nndN<-nnIndex(finpines,id=paste("T",1:126,sep=""),N=1,
              smark="diameter",buffer=TRUE,buf.xwid =2,buf.ywid=3)
nndN
nndhN<-nnIndex(finpines,id=paste("T",1:126,sep=""),N=4,
       smark=c("diameter","height"),buffer=TRUE,buf.xwid =2
       ,buf.ywid=3)
nndhN

# Only the points in the core zone (Removed the points in the buffer zone)
nndN_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),N=4,
              smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndN_core
nndhN_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),N=4,
         smark=c("diameter","height"),buffer=FALSE,buf.xwid =2,
         buf.ywid=3)
nndhN_core

## Remove some points, and then identify the marks of Nearest Neighbour
nndN.rm_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),
                    rm.id=c("T1","T3","T8","T9","T59","T60","T120"),
            N=4,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndN.rm_core

## add some points, and then identify the marks  of Nearest Neighbour
add.x=c(-2,-1,0,2,4)
add.y=c(-4,-3,-6,0,1)
add.marks=data.frame(diameter=c(2.0,3.0,4.0,5.0,6.0),
                     height=c(2.5,3.5,4.5,5.5,6.5))
add.Xdata=ppp(x=add.x,y=add.y,marks=add.marks,c(-5,5),c(-8,2))

nndN.add_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),
                       add.X=add.Xdata,
                       add.id=paste("NT",1:5,sep=""),
          N=4,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndN.add_core

#### Based on specific circular (R=5) of nearest neighbour####
## Basic usage
nndR<-nnIndex(finpines,id=paste("T",1:126,sep=""),R=0.5,
              smark="diameter",buffer=TRUE,buf.xwid =2,buf.ywid=3)
nndR
nndhR<-nnIndex(finpines,id=paste("T",1:126,sep=""),R=0.5,
              smark=c("diameter","height"),buffer=TRUE,
               buf.xwid =2,buf.ywid=3)
nndhR

# Only the points in the core zone (Removed the points in the buffer zone)
nndR_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),R=0.5,
            smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndR_core
nndhR_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),R=0.5,
            smark=c("diameter","height"),buffer=FALSE,
                    buf.xwid =2,buf.ywid=3)
nndhR_core

## Remove some points, and then identify the marks  of Nearest Neighbour
nndR.rm_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),
                  rm.id=c("T1","T3","T8","T9","T59","T60","T120"),
          R=0.5,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndR.rm_core

## add some points, and then identify the marks  of Nearest Neighbour
nndR.add_core<-nnIndex(finpines,id=paste("T",1:126,sep=""),
                       add.X=add.Xdata,
                       add.id=paste("NT",1:5,sep=""),
          R=0.5,smark="diameter",buffer=FALSE,buf.xwid =2,buf.ywid=3)
nndR.add_core
}
