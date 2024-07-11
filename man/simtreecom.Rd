\name{simtreecom}
\alias{simtreecom}
\title{
Simulated a forest community data.
}
\description{
Simulated a forest community data.
}
\usage{
simtreecom(size,nspe,nspe_max,xrange,
           yrange,dbhrange,type="com",lambda=1)
}
\arguments{
  \item{size}{
Pool of species
}
  \item{nspe}{
Number of species
}
  \item{nspe_max}{
Maximum number of species
}
  \item{xrange}{
Range of X axes
}
  \item{yrange}{
Range of Y axes
}
  \item{dbhrange}{
Range of DBH
}
  \item{type}{
Data type, "ppp" or "com"
}
  \item{lambda}{
Degree of coordinate aggregation
}
}
\value{
Data frame of forest community data
}
\references{
NULL
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
treecom<-simtreecom(size=10,nspe=10,nspe_max=36,xrange=c(0,100),
           yrange=c(0,100),dbhrange=c(5,50),type="com",lambda=1)
treecom
}




