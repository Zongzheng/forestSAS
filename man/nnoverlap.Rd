\name{nnoverlap}
\alias{nnoverlap}
\title{
Crown overlap among nearest neighbour
}
\description{
Identify wheter crown overlap among nearest neighbour.
}
\usage{
nnoverlap(nncrown, nndist)
}
\arguments{
  \item{nncrown}{
Crown width of nearest neighbour.
}
  \item{nndist}{
Distance of nearest neighbour.
}
}
\value{
Crown overlap among nearest neighbour,the value is positive means overlap,
value is negative means no overlap.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
data(tree.ppp)
NNcrown<-nnIndex(tree.ppp,N=4,smark="crownwid.m",buffer=FALSE)
NNoverlap<-nnoverlap(NNcrown$nncrownwid.m,NNcrown$nndist)
NNoverlap
}