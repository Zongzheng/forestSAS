\name{expandedge}
\alias{expandedge}
\title{
Expand the edges for the point pattern with replication.
}
\description{
When computing forest spatial structure indices, The trees near the edges of the study region are distorted because the outside is empty. Common solutions to this problem are not to use indices computed for trees near the edges, or (with rectangular regions) to attach translated copies, thus changing the topology into a torus.
}
\usage{
expandedge(X, xwidth, ywidth, id = 1:X$n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A point pattern (object of class "ppp").
}
  \item{xwidth}{
Distance from the edges x coordinates,note xwidth is the half of your width wanted, because the width will expand both edges in the x coordinates.
}
  \item{ywidth}{
Distance from the edges y coordinates,note xwidth is the half of your width wanted, because the width will expand both edges in the y coordinates.
}
  \item{id}{
Specific identification number of the points in the point pattern.
}
}
\details{
When expanding for the full pattern, and using expandedge() with a positive  xwidth and ywidth; conversely, shrinking for the full pattern, and using shrinkedge () with a negative xwidth and ywidth.
}
\value{
Result returns original data (not a a point pattern with the same structure as trees), id is new id of points after edges expanded and old.id is orginal id of points.
The pattern is first expanded by surrounding it with 8 shifted copies (the window must be rectangular). Then, the parts of the pattern that are at a distance less than xwidth or ywidth from an edge of the enlarged pattern are discarded.
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

# Expand the rectangle [-5,5]x[-8,2] to [-6,6]x[-9,3]
Expand.trees<- expandedge(finpines,xwidth=2,ywidth=1,id=1:126)
Expand.trees

# Show the changes by figures
opar<-par(mfrow=c(1,2))
plot(finpines$x,finpines$y)
text(finpines$x,finpines$y,1:126)
plot(Expand.trees$x,Expand.trees$y)
text(Expand.trees$x,Expand.trees$y,Expand.trees$old.id)
rect(-5,-8,5,2,border="red")
par(opar)
}
