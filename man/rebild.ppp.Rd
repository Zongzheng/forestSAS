\name{rebild.ppp}
\alias{rebild.ppp}
\title{
Reestablishing an object of class "ppp"
}
\description{
Reestablishing an object of class "ppp" by adding or removing some data from orginal point pattern (object of class "ppp")
}
\usage{
rebild.ppp(X, id = 1:(X$n), rm.id = NULL, add.id = NULL, add.x = NULL, add.y = NULL, add.mark = NULL)
}
\arguments{
  \item{X}{
A point pattern (object of class "ppp").
}
  \item{id}{
Specific identification number of each row in the point pattern
}
  \item{rm.id}{
Needed removed the specific rows in the point pattern, refer to the id.
}
  \item{add.id}{
Needed added the specific rows in the point pattern.
}
  \item{add.x}{
Vector of x coordinates of data points for add.id
}
  \item{add.y}{
Vector of y coordinates of data points for add.id
}
  \item{add.mark}{
Mark values for the points for add.id. A vector or data frame.
}
}
\details{
}
\value{
An object of class "ppp" describing a point pattern in the two-dimensional plane (see ppp.object), which was Reestablished by adding or removing some data from orginal point pattern
}
\author{
Zongzheng Chai, chaizz@126.com
}
\seealso{
R function ppp in the package "spatstat"
}
\examples{
####Based on the simulated data####
# Creating a simulated point pattern
x <- runif(20)
y <- runif(20)
X <- ppp(x, y, c(0,1), c(0,1))
X

# Adding a identification number (ID) for each data point in the point pattern
# the following are equivalent
X1<-rebild.ppp(X)
X1<-rebild.ppp(X,id=1:X$n)
X1<-rebild.ppp(X,id=1:20)
X1
as.data.frame(X1)

# Adding a identification number (ID) for each data point in the point pattern, and
# Removing the assinged points according to the ID.
X2<-rebild.ppp(X,id=1:20,rm.id=1:5)
X2
as.data.frame(X2)
# Adding some points into orginal point pattern
newx <- runif(10)
newy <- runif(10)
as.data.frame(newX1)
XY<-rebild.ppp(X,add.x=newx,add.y=newy,add.id=paste("new",1:10,sep=""))
XY
as.data.frame(XY)

####Based on the example data####
data(finpines)
finpines
# Marked planar point pattern: 126 points
# Mark variables: diameter, height
# window: rectangle = [-5, 5] x [-8, 2] metres

# Adding a identification number (ID) for each tree in the finpines
# finpines$n
# [1] 126
finpines_id<-rebild.ppp(finpines,id=paste("T",1:126,sep=""))
finpines_id
head(as.data.frame(finpines_id))

# Removing the trees with id "T1","T3","T8","T9","T59","T60","T120"
finpines_rm<-rebild.ppp(X=finpines,id=paste("T",1:126,sep=""),
                          rm.id=c("T1","T3","T8","T9","T59","T60","T120"))
finpines_rm
as.data.frame(finpines_rm)

# Adding some trees data into finpines
marks=data.frame(diameter=c(2.0,3.0,4.0,5.0,6.0),
                 height=c(2.5,3.5,4.5,5.5,6.5))
finpines_add<-rebild.ppp(X=finpines,id=paste("T",1:126,sep=""),
                         add.x=c(-2,-1,0,2,4),add.y=c(-4,-3,-6,0,1),
            add.id=paste("NT",1:5,sep=""),
            add.mark=marks)
finpines_add
as.data.frame(finpines_add)
}





