\name{treedata}
\alias{treedata}
\docType{data}
\title{
Sample data for analizing the forest spatial structure.
}
\description{
A sample data of field survey, to help us understanding the method of forest spatial structure analysis.
}
\usage{data("treedata")}
\format{
  A data frame with 41 observations on the following 11 variables.
  \describe{
    \item{\code{tree.id}}{Specific identification number of trees in the point pattern}
    \item{\code{x}}{x coordinates of trees}
    \item{\code{y}}{y coordinates of trees}
    \item{\code{sp.code}}{Specific code of tree species in the point pattern}
    \item{\code{dbh.cm}}{Diameter at breast height (DBH),1.3 m. unit:cm}
    \item{\code{h.m}}{Tree height,unit:m}
    \item{\code{storey}}{forest storey}
    \item{\code{crownwid.m}}{Crown width,unit:m}
    \item{\code{group}}{Tree group, which can be divided exotic,pioneer,transtional,climax,5 types}
    \item{\code{biomass.kg}}{Tree biomass,unit:kg}
    \item{\code{quality}}{Quality of individual trees}
  }
}
\references{
None
}
\examples{
data(treedata)
}
