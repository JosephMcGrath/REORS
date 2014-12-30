\name{KMeans}
\alias{KMeans}

\title{
Applies crisp k-means classification to an input Raster* object.
}

\description{
Uses the standard k-means algorithm, with some additional customisation available.
}

\usage{
KMeans(rasterIn, nCentres = 10, fileName = tempfile(), itts = 1, breakCon = 5, distM = "euc", silent = TRUE, interPlot = FALSE)
}

\arguments{
  \item{rasterIn}{Name of the image file to classify. Maybe run it through a cleaning function first?}
  \item{nCentres}{How many clusters should the data be split into? Empty clusters are deleted as the algorithm progresses - may change this later.}
  \item{fileName}{Name to write file to, defaults to temporary file.}
  \item{itts}{Maximum number of iterations to run the algorithm for.}
  \item{breakCon}{How little variation between iterations will break the loop early not very well implemented yet - just sum of distances.}
  \item{distM}{Distance measure to calculate memberships: "euc" = euclidean distance "man" = Manhattan distance "eu2" = squared euclidean distance}
  \item{silent}{Should details of the classification be output as it works?}
  \item{interPlot}{Should the classification be plotted each iteration?}
}

\value{Image (also written to disk) with classes represented by numerical values.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- KMeans(r, nCentres = 4)
r
}
