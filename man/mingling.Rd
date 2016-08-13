\name{mingling}
\alias{mingling}
\title{
A forest spatial structure index characterizing tree diversity
}
\description{
The tree attribute mingling describes the species variety in the vicinity of a given reference tree and has been defined as the proportion of the n nearest neighbours
that do not belong to the same species
}
\usage{
mingling(x)
}
\arguments{
  \item{x}{
A vector composed by 5 tree species
}
}
\value{
Result will retrun five values,0,0.25,0.5,0.75,1,which means the propertion of 
the same tree species with refercence tree in the 4 nearest neighbours 
}
\references{
Hui GY, Zhao X, Zhao Z, Gadow Kv. 2011. Evaluating tree species spatial diversity based on neighborhood relationships. Forest Sci 57:292-300
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
spname1<-c("sp1","sp1","sp1","sp1","sp1")
spname2<-c("sp1","sp1","sp1","sp1","sp2")
spname3<-c("sp1","sp1","sp1","sp3","sp2")
spname4<-c("sp1","sp1","sp4","sp3","sp2")
spname5<-c("sp1","sp5","sp4","sp3","sp2")

mingling1<-mingling(spname1)
mingling1
mingling2<-mingling(spname2)
mingling2
mingling3<-mingling(spname3)
mingling3
mingling4<-mingling(spname4)
mingling4
mingling5<-mingling(spname5)
mingling5
}
