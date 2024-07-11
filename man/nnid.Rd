\name{nnid}
\alias{nnid}
\title{
Find the id of nearest neighbour for each point.
}
\description{
Find the id of nearest neighbour for each point in a point pattern by specific numbers or  circular neighborhood.
}
\usage{
nnid(X, N = NULL, R = NULL, id, exclude = TRUE)
}
\arguments{
  \item{X}{
A marked point pattern (object of class "ppp").
}
  \item{N}{
Specific number of points in the neighborhood, the value is always assinged 4.
}
  \item{R}{
Specific circular neighborhood, the value is always assinged 5.
}
  \item{id}{
Specific identification number of each row in the point pattern.
}
  \item{exclude}{
Wether including id of core points,
if exclude is TRUE the result return the data removed the id of core points,
if exclude is FALSE the result return the data with the id of core points.
}
}
\value{
id of nearest neighbour for each point
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
library(spatstat)
data(finpines)
#Find the id of the nearest neighbour in a certain numbers and without id
finpines.rmidN4<-nnid(finpines,id=paste("T",1:finpines$n),N=4)
finpines.rmidN4

#Find the id of the nearest neighbour in a certain numbers with id
finpines.idN4<-nnid(finpines,id=paste("T",1:finpines$n),N=4,exclude=FALSE)
finpines.idN4

#Find the id of the nearest neighbour within a certain radius without id
data(tree.ppp)
finpines.rmidR0.5<-nnid(tree.ppp,id=paste("T",1:41),R=0.5)
finpines.rmidR0.5

#Find the id of the nearest neighbour within a certain radius with id
finpines.idR0.5<-nnid(tree.ppp,id=paste("T",1:41),R=0.5,exclude=FALSE)
finpines.idR0.5
}


