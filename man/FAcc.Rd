\name{Facc}
\alias{Facc}

\title{
Fuzzy accuracy measure.
}

\description{
Calculates accuracy of a fuzzy classification compared to crisp reference    <-- This section is poorly worded. values.
The accuracy are calculated depending on membership values - if the fuzzy membership values relate to the reference class then accuracy should increase with membership values.
May also be used alongside Defuzzify to plot how the membership values change within each class.
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
