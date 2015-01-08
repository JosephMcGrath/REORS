\name{KMeans}
\alias{KMeans}

\title{
Applies crisp k-means classification to an input Raster* object.
}

\description{
Uses the standard k-means algorithm, with some additional customisation available.
}

\usage{
KMeans(rasterIn, nCentres = 10, its = 1, weight = 1, fileOut = tempfile(pattern = "REORS"), breakCon = 0.01, standIn = FALSE, distM = "euc", silent = TRUE, interPlot = FALSE)
}

\arguments{
  \item{rasterIn}{Name of the image file to classify. Maybe run it through a cleaning function first?}
  \item{nCentres}{How many clusters should the data be split into? Empty clusters are deleted as the algorithm progresses - may change this later.}
  \item{fileOut}{Name to write file to, defaults to temporary file.}
  \item{its}{weight: The weights to apply to each layer (higher weight means an greater importance to clustering, can be integers or decimals. Must be a single values or have length equal to the number of input layers.}
  \item{breakCon}{How little variation between iterations will break the loop early calculated as an average per variable, assuming variables are between 0-1.}
  \item{standIn}{Should the data be locked between 0 and 1 before classification?}
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
