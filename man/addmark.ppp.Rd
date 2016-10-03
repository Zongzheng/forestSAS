\name{addmark.ppp}
\alias{addmark.ppp}
\title{
Add marks for a point pattern
}
\description{
Add marks for a point pattern
}
\usage{
addmark.ppp(X, add.mark, add.name = "storey")
}
\arguments{
  \item{X}{
A point pattern (object of class "ppp").
}
  \item{add.mark}{
Marks need added in the point pattern
}
  \item{add.name}{
Names of added marks (add.mark)
}
}
\value{
A point pattern added marks
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
}
