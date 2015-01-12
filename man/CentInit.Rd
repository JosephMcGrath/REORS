\name{CentInit}
\alias{CentInit}

\title{
Several methods of generating initial centres for a centroid-based clustering algorithm.
}

\description{
Several methods of generating initial centres for a centroid-based clustering algorithm. A few are also implemented as part of the algorithms, but the more time-intenisve ones are isolated here.
}

\usage{
CentInit(rasterIn, nCentres, meth = "rand", silent = TRUE){
}

\arguments{
  \item{rasterIn}{The Raster* object to use, passed through RasterLoad.}
  \item{nCentres}{How many clusters should the data be split into?}
  \item{meth}{The method used to initialise centres, one of: "rand" : A simple random distribution between the minimum and maximum values per layer. "lin"  : Distributed linearly between the minimum and maximum values per layer. "prin" : (NYI) Linear distribution along the first principal component. "dist" : Distributed randomly, but with probability based on data in.}
  \item{silent}{Should the function work without progress reports?}
}

\value{A matrix of cluster centres with columns as centres and rows as layers.}

\author{Joseph McGrath}

\examples{
r <- brick(ncol = 50, nrow = 50, nl = 2)
values(r) <- sample(1:5000, 5000, TRUE)
r

CentInit(r, 5)
}
