\name{forestSAS-package}
\alias{forestSAS-package}
\alias{forestSAS}
\docType{package}
\title{
\packageTitle{forestSAS}
}
\description{
\packageDescription{forestSAS}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{forestSAS}
\packageIndices{forestSAS}
Forest structure commonly refers to a distribution pattern of tree attributes within a forest ecosystem. Similarly, tree population structure describes the distribution characteristics of individuals of conspecifics within a community, and the spatial structure of a tree population is largely determined by the relationships among neighboring groups of trees.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
None
}
\examples{
data(tree.ppp)
##Get the tree attributies of nearest neighbour for calculation
nnindices<-nnIndex(tree.ppp,N=4,
                  smark=c("sp.code","dbh.cm","storey","crownwid.m","group",
                           "biomass.kg","quality","x","y"),buffer=FALSE)

#Species mingling
M<-fsasN4(nnindices$nnsp.code,match.fun=mingling)
M
#Stand storey differation degree
H<-fsasN4(nnindices$nnstorey,match.fun=differ)
H
#Tree dominance
U<-fsasN4(nnindices$nndbh.cm,match.fun=dominance)
U
#Tree successional degree
S<-fsasN4(nnindices$nngroup,match.fun=ideal,para="Climax")
S
#Tree biomass dominance
B<-fsasN4(nnindices$nnbiomass.kg,match.fun=dominance)
B
#Tree quality ideal state
Q<-fsasN4(nnindices$nnquality,match.fun=ideal,para=c("Excellent","Good"))
Q
#Tree corwding degree
D<-fsasN4(nnoverlap(nnindices$nncrownwid.m,nnindices$nndist),match.fun=crowding)
D
#Tree uniform angle index
W<-fsasN4(nnangle(nnindices$nndist,nnindices$nnx,
      nnindices$nny)$nnangle,match.fun=uniform.angle,para=72)
W
}
