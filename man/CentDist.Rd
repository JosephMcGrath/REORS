\name{CentDist}
\alias{CentDist}

\title{
Calculates the distance between two sets of cluster centres.
}

\description{
Calculates the distance between two sets of cluster centres. Currently only uses cluster centre proximity index (CCPI). More planned in the future.
}

\usage{
CentDist(centres1, centres2)
}

\arguments{
  \item{centres1}{The first set of cluster centres, with columns for centres and rows for input layers.}
  \item{centres2}{The second set of centres, order does not matter but must have the same dimensions at centres1.}
}

\value{Distance measures between the two provided sets of centres.}

\references{
Capitaine H.L., Frelicot C. (2011)
  A fast fuzzy c-means algorithm for color image segmentation
  Proceedings of European Society for Fuzzy Logic and Technology (EUSFLAT'2011) 1074-1081.
}

\author{Joseph McGrath}

\examples{
c1 <- matrix(runif(30, 0, 1), ncol = 5)
c2 <- matrix(runif(30, 0, 1), ncol = 5)

CentDist(c1, c2)
}
