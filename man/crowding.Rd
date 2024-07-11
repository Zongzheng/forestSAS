\name{crowding}
\alias{crowding}
\title{
A forest spatial structure index characterizing crowding degree.
}
\description{
Analyze the crowding degree of a neighborhood unit according to the overlapping of the crown in spatial micro-environment which clearly define the crowding degree for a reference tree and its four nearest neighbors, and to some degree of the competition pressure on it as well.
}
\usage{
crowding(x)
}
\arguments{
  \item{x}{
A vector composed by 5 crowding degree value
}
}
\value{
Result will retrun five values,0,0.25,0.5,0.75,1,which means none, one, two, three, or four overlap of nearest neighbors with reference tree,respectively.
}
\references{
Hu YB, Hui GY. 2015. How to describe the crowding degree of trees based on the relationship of neighboring trees. Journal of Beijing Forestry University. 37(9):1-8.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
overlap1<-c(-0.5,-0.4,-0.3,-0.2) ##negative means no overlap
overlap2<-c(-0.5,-0.4,-0.3,0.2)  ##positive means overlap
overlap3<-c(-0.5,-0.4,0.3,0.2)
overlap4<-c(-0.5,0.4,0.3,0.2)
overlap5<-c(0.5,0.4,0.3,0.2)

crowding1<-crowding(overlap1)
crowding1
crowding2<-crowding(overlap2)
crowding2
crowding3<-crowding(overlap3)
crowding3
crowding4<-crowding(overlap4)
crowding4
crowding5<-crowding(overlap5)
crowding5
}
