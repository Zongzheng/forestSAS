\name{fsasN4}
\alias{fsasN4}
\title{
A body function to analyze the forest saptial structure.
}
\description{
The function provided a easy flexible funciton body to analyze the forest saptial structure.
}
\usage{
fsasN4(nnattri, match.fun, para = NULL)
}
\arguments{
  \item{nnattri}{
  attributies of nearest neighbour
}
  \item{match.fun}{
  funcitons,such as "differ","dominance","ideal","uniform.angle","crowding","mingling",etc.
}
  \item{para}{
  the parameter in the above match.fun
}
}
\value{
result is the "nnmark" and,colum I is the sptial structre indices value
Icount is the count distribution  of I
Ifreq is the frequency distribution of I
meanI is the average value of I
data is the final point pattern
}
\references{
Gadow Kv, and Hui G. 2002. Characterizing forest spatial structure and diversity. Proc of the SUFOR international workshop: Sustainable forestry in temperate regions. p 20-30.
}
\author{
Zongzheng Chai, chaizz@126.com
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
W<-fsasN4(nnangle(nnindices$nndist,nnindices$nnx,nnindices$nny)$nnangle,
          match.fun=uniform.angle,para=72)
W
}
