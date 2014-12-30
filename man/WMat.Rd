\name{WMat}
\alias{WMat}

\title{
Kernel creation tool for focal calculations.
}

\description{
Has a number of pre-set shapes of matrices 
}

\usage{
WMat(matSize, type = "circle")
}

\arguments{
  \item{matSize}{The size to use for the edges of the matrix.}
  \item{type}{Shape to use for the matrix: "circle" an approximately circular matrix (default). "square" for a standard square matrix.}}

\value{A matrix of weights, either 1 or NA}

\author{Joseph McGrath}

\examples{
WMat(5)
WMat(5, "square")
WMat(9)
}
