\name{uniform.angle}
\alias{uniform.angle}
\title{
A forest spatial structure index characterizing tree spatial
distribution patterns
}
\description{
The uniform angle index (UAI) is used to characterize the spatial distribution of a forest community or of individual tree species
within that community.By gradually comparing the included  4 angles with the standard angle (72).
}
\usage{
uniform.angle(x, para)
}
\arguments{
  \item{x}{
Moving clockwise around reference tree,4 angle were obtained among adjacent neighbours
}
  \item{para}{
standard angle (72)
}
}
\value{
Result will retrun five values,0,0.25,0.5,0.75,1,which means
none, one, two, three, or four angles are smaller than 72,
respectively.
}
\references{
Zhao ZH, Hui GY, Hu YB, Wang HX, Zhang GQ, and von Gadow K. 2014. Testing the significance of different tree spatial distribution patterns based on the uniform angle index. Canadian Journal of Forest Research 44:1419-1425.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
angle1<-c(80,80,80,80)
angle2<-c(80,80,80,70)
angle3<-c(80,80,60,70)
angle4<-c(80,50,60,70)
angle5<-c(40,50,60,70)

uai1<-uniform.angle(angle1,para=72)
uai1
uai2<-uniform.angle(angle2,para=72)
uai2
uai3<-uniform.angle(angle3,para=72)
uai3
uai4<-uniform.angle(angle4,para=72)
uai4
uai5<-uniform.angle(angle5,para=72)
uai5
}
