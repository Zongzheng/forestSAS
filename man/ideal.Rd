\name{ideal}
\alias{ideal}
\title{
A forest spatial structure index characterizing distribution of ideal state for tree attributies.
}
\description{
Analyze the distribution of ideal state for tree attributies in a neighborhood unit according to the characteristics of the tree attributies,such as tree health,climax species distribution,etc.in spatial micro-environment.which clearly define the ideal degree of tree attributies for a reference tree and its four nearest neighbors.
}
\usage{
ideal(x, para)
}
\arguments{
  \item{x}{
A vector composed by 5 tree attributies.
}
  \item{para}{
Ideal state of tree attributies.
}
}
\value{
Result will retrun five values,0,0.25,0.5,0.75,1,which means 
none,one, two, three or four ideal tree attributies of nearest neighbors 
for reference tree,respectively.
}
\references{
None
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
health1<-c("poor","poor","poor","poor")
health2<-c("poor","poor","poor","excellent")
health3<-c("poor","poor","good","excellent")
health4<-c("poor","excellent","good","excellent")
health5<-c("good","excellent","good","excellent")

ideal1<-ideal(health1,para=c("good","excellent"))
ideal1
ideal2<-ideal(health2,para=c("good","excellent"))
ideal2
ideal3<-ideal(health3,para=c("good","excellent"))
ideal3
ideal4<-ideal(health4,para=c("good","excellent"))
ideal4
ideal5<-ideal(health5,para=c("good","excellent"))
ideal5
}


