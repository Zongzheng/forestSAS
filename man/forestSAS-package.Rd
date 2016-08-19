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
#Tree successional degree
S<-fsasN4(nnindices$nngroup,match.fun=ideal,para="Climax")
S
#Tree quality ideal state
Q<-fsasN4(nnindices$nnquality,match.fun=ideal,para=c("Excellent","Good"))
Q
#Tree corwding degree
C<-fsasN4(nnoverlap(nnindices$nncrownwid.m,nnindices$nndist),match.fun=crowding)
C
#Tree uniform angle index
W<-fsasN4(nnangle(nnindices$nndist,nnindices$nnx,nnindices$nny)$nnangle,
          match.fun=uniform.angle,para=72)
W
#Tree dominance
U<-fsasN4(nnindices$nndbh.cm,match.fun=dominance)
U
#Tree biomass dominance
B<-fsasN4(nnindices$nnbiomass.kg,match.fun=dominance)
B

##Compute the structrue heterogeneity of index
M.fsh<-fsh(M$result$index,optm=1)
M.fsh
H.fsh<-fsh(H$result$index,optm=1)
H.fsh
S.fsh<-fsh(S$result$index,optm=1)
S.fsh
Q.fsh<-fsh(Q$result$index,optm=1)
Q.fsh
C.fsh<-fsh(C$result$index,optm=0.5)
C.fsh
W.fsh<-fsh(W$result$index,optm=0.5)
W.fsh
U.fsh<-fsh(U$result$index,optm=0)
U.fsh
B.fsh<-fsh(B$result$index,optm=0)
B.fsh

##Compute total forest saptial structrue heterogeneity
Total.fsh<-fsh(index=c(M$meanI,H$meanI,S$meanI,Q$meanI,
                       C$meanI,W$meanI,U$meanI,B$meanI),
               optm=c(1,1,1,1,0.5,0.5,0,0))
Total.fsh
}
