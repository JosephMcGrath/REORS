\name{Facc}
\alias{Facc}

\title{
Fuzzy accuracy measure.
}

\description{
Visualises the relationship between membership values and reference classes. \cr
Takes fuzzy membership values from a classification, along with reference classes to compare to. Using this, the accuracy is calculated at a range of membership values (assuming a pixel is assigned to the class it has the highest membership to.\cr
Where memberships correspond well to a reference class, the accuracy should increase with membership.
}

\usage{
FAcc(classed, reference, bins = 100, plotOut = FALSE, silent = TRUE)
}

\arguments{
  \item{classed}{The fuzzy membership values of the classification to be assessed.}
  \item{reference}{The single-layer raster object that represents crisp memberships the classification is to be compared to.}
  \item{plotOut}{If the resulting values should be automatically plotted.}
  \item{bins}{The number of bins to calculate accuracy for.}
  \item{silent}{Should details of the classification be output as it works?}
}

\value{A matrix of values, with rows for each class and columns for membership values.}

\author{Joseph McGrath}

\examples{

}
