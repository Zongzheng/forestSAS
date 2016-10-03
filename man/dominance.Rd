\name{dominance}
\alias{dominance}
\title{
A forest spatial structure index characterizing tree dominance
}
\description{
The tree attribute dominance was proposed by Hui et al. (1998) to relate the relative dominance of a given tree to its species or silvicultural significance. It is defined as the proportion of
the n nearest neighbours of a given reference tree which are bigger than the reference tree.
}
\usage{
dominance(x)
}
\arguments{
  \item{x}{
A vector composed by 5 tree attributes, such as dbh,biomass.
}
}
\value{
Result will retrun five values,0,0.25,0.5,0.75,1,which means the propertion of
the larger tree attributes in the 4 nearest neighbours than the reference tree.
}
\references{
Gadow Kv, and Hui GY. 2002. Characterizing forest spatial structure and diversity. Proc of the SUFOR international workshop: Sustainable forestry in temperate regions. p 20-30.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
dbh1<-c(5,4,4,4,4)
dbh2<-c(5,4,4,4,6)
dbh3<-c(5,4,4,7,6)
dbh4<-c(5,4,8,7,6)
dbh5<-c(5,9,8,7,6)

dominance1<-dominance(dbh1)
dominance1
dominance2<-dominance(dbh2)
dominance2
dominance3<-dominance(dbh3)
dominance3
dominance4<-dominance(dbh4)
dominance4
dominance5<-dominance(dbh5)
dominance5
}
