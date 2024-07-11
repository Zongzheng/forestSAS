\name{nnangle}
\alias{nnangle}
\title{
Identify the angle among nearest neighbours
}
\description{
Describes the degree of regularity in the spatial distribution of n trees that are nearest to a reference tree. Moving clockwise around reference tree,the angle was obtained between two adjacent neighbours.
}
\usage{
nnangle(nndist, nnx, nny)
}
\arguments{
  \item{nndist}{
Distance of nearest neighbour
}
  \item{nnx}{
x coordinate of nearest neighbour
}
  \item{nny}{
y coordinate of nearest neighbour
}
}
\value{
The angles obtained between two adjacent neighbours.
}
\references{
None
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
data(tree.ppp)
NNcoord<-nnIndex(tree.ppp,N=4,smark=c("x","y"),buffer=FALSE)
NNangle<-nnangle(NNcoord$nndist,NNcoord$nnx,NNcoord$nny)
NNangle
}
