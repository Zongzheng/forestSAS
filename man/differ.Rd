\name{differ}
\alias{differ}
\title{
A forest spatial structure index characterizing differation degree of tree attributies.
}
\description{
Analyze the differation degree of tree attributies in a neighborhood unit according to the characteristics of the tree attributies,such as forest storey,speices,etc.in spatial micro-environment.which clearly define the differation degree of tree attributies for a reference 
tree and its four nearest neighbors.
}
\usage{
differ(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector composed by 5 tree attributies.
}
}
\value{
Result will retrun five values,0,0.25,0.5,0.75,1,which means 
one, two, three, four or five different attributies of nearest neighbors 
for reference tree,respectively.
}
\references{
None
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
##S1,S2,S3,S4,S5,represent 5 different forest stoeries
storey1<-c("S1","S1","S1","S1","S1")
storey2<-c("S1","S1","S1","S1","S2")
storey3<-c("S1","S1","S1","S3","S2")
storey4<-c("S1","S1","S4","S3","S2")
storey5<-c("S1","S5","S4","S3","S2")

differ1<-differ(storey1)
differ1
differ2<-differ(storey2)
differ2
differ3<-differ(storey3)
differ3
differ4<-differ(storey4)
differ4
differ5<-differ(storey5)
differ5
}
