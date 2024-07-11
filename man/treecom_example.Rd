\name{treecom_example}
\alias{treecom_example}
\docType{data}
\title{
Example data for analizing the forest community.
}
\description{
A example data of field survey, to help us understanding the method of forest spatial structure analysis.
}
\usage{data("treecom_example")}
\format{
  A data frame
  \describe{
    \item{\code{id}}{Specific identification number of trees}
    \item{\code{x}}{x coordinates of trees}
    \item{\code{y}}{y coordinates of trees}
    \item{\code{spe}}{Specific code of tree species in the point pattern}
    \item{\code{dbh}}{Diameter at breast height (DBH),1.3 m. unit:cm}
    \item{\code{h}}{Tree height,unit:m}
    \item{\code{storey}}{forest storey}
    \item{\code{cw}}{Crown width,unit:m}
    \item{\code{quality}}{Quality of individual trees}
  }
}
\references{
None
}
\examples{
data(treecom_example)
treecom_example
}
