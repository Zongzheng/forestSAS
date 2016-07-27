\name{list_to_matrix}
\alias{list_to_matrix}
\title{
Converting list to  matrix
}
\description{
Converting the data with class list to  the data with class matrix
}
\usage{
list_to_matrix(x, item = NA)
}
\arguments{
  \item{x}{
Data with class list
}
  \item{item}{
After converting list to  matrix, using NA to pad out the blanks within matrix
}
}
\value{
Data with class matrix
}
\author{
Zongzheng Chai,chaizz@126.com
}
\examples{
# Creating a list data "datalist", and covert "datalist" to a matrix data "datamat"
datalist<-list(dataA=1:5,dataB=1:10,dataC=1:15,dataD=1:20)
datamat<-list_to_matrix(datalist)
datamat
}
