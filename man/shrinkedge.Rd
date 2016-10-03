\name{shrinkedge}
\alias{shrinkedge}
\title{
Shrink the edges for the point pattern
}
\description{
Sometimes, the window of point pattern is too large, we can shrink its edges to the wanted window of point pattern
}
\usage{
shrinkedge(X, xwidth, ywidth, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A point pattern (object of class "ppp").
}
  \item{xwidth}{
Distance from the edges x coordinates,note xwidth is the half of your width wanted, because the width will shrink both edges in the x coordinates.
}
  \item{ywidth}{
Distance from the edges y coordinates,note xwidth is the half of your width wanted, because the width will shrink both edges in the y coordinates.
}
  \item{id}{
Specific identification number of the points in the point pattern.
}
}
\details{
When expanding for the full pattern, and using expandedge() with a positive  xwidth and ywidth; conversely, shrinking for the full pattern, and using shrinkedge () with a negative xwidth and ywidth,the parts of the pattern that are at a distance less than -width from an edge are discarded.
}
\value{
Result returns original data (not a a point pattern with the same structure as trees), id is new id of points after edges expanded and old.id is orginal id of points.
If xwidth or ywidth = 0, trees are returned unchanged.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
library(spatstat)
data(finpines)
finpines$window
# window: rectangle = [-5, 5] x [-8, 2] metres

#Shrink the rectangle [-5,5]x[-8,2] to [-3,3]x[-5,-1]
shrink.trees<- shrinkedge(finpines,xwidth=2,ywidth=3,id=1:126)
shrink.trees

# Show the changes by figures
opar<-par(mfrow=c(1,2))
plot(finpines$x,finpines$y)
text(finpines$x,finpines$y,1:126)
rect(-3,-5,3,-1,border="red")
plot(shrink.trees$x,shrink.trees$y)
text(shrink.trees$x,shrink.trees$y,shrink.trees$old.id)
par(opar)
}

