\name{storeydvd}
\alias{storeydvd}
\title{
Dividing the forest storeies
}
\description{
Forest overstory can be divided the many stories by dividing the range of tree height.
}
\usage{
storeydvd(height, minH = min(height), maxH = max(height),
      storeynum = 6, include.lowest = TRUE, right = TRUE)
}
\arguments{
  \item{height}{
Data of tree height
}
  \item{minH}{
Minimum value of height
}
  \item{maxH}{
Maximum value of height
}
  \item{storeynum}{
Number of storey
}
  \item{include.lowest}{
Logical, indicating if an "[i]" equal to the lowest (or highest, for right=FALSE) "breaks" value should be included. See the function "cut".
}
  \item{right}{
Logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.See the function "cut".
}
}
\value{
heightfreq is the frequency of tree height
heightdata is the result after divided the storey
}
\references{
None
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
library(spatstat)
data(finpines)
finpinesdata<-as.data.frame(finpines)
####Dividing the stories
finpines.storey<-storeydvd(finpinesdata$height,storeynum=6)
finpines.storey

####Computing the storey differation
##Add the storey mark for finepines
finpines.addstorey<-addmark.ppp(finpines,
              add.mark=finpines.storey$heightdata[,2:3],
                    add.name=c("interval","storey" ))
finpines.addstorey
##Compute the storeies of nearest neighbour in the point pattern
finpines.nnstorey<-nnIndex(finpines.addstorey,smark="storey",N=4,
                           buf.xwid =2,buf.ywid=3)
##Compute the stoery differation
finpines.H<-fsasN4(finpines.nnstorey$nnstorey,match.fun=differ)
finpines.H
}

