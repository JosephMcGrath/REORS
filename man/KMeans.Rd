\name{KMeans}
\alias{KMeans}

\title{
Applies crisp k-means classification to an input Raster* object.
}

\description{
Uses the fuzzy c-means algorithm to attempt to classify the input image, with some additional customisation available. Iteratively re-assigns pixels to classes in an attempt to optimise the divisions between pixels. The algorithm is heavily influenced by the initial cluster centres.
}

\usage{
KMeans(rasterIn, nCentres = 10, its = 1, weight = 1, init = "lin", breakCon = 0.01, standIn = FALSE, distM = "euc", randRe = FALSE, fileOut = tempfile(pattern = "REORS - "), silent = TRUE, interPlot = FALSE)
}

\arguments{
  \item{rasterIn}{Raster objects to be classified. Ran through RasterLoad.}
  \item{nCentres}{Number of clusters to split the data into. Ignored if "init" is uses pre-set centres.}
  \item{its}{Maximum number of iterations to run the algorithm for.}
  \item{weight}{The weights to apply to each layer (higher weight means an greater importance to clustering, can be integers or decimals. Must be a single value or have length equal to the number of input layers.}
  \item{init}{How should the clusters be initialised? Current methods are: "lin": Linearly, from the minimum value in each layer, to the maximum. "rand": Randomly within the minimum/maximum available values. Matrix of centres, one column for each centre, one row for each layer.}
  \item{breakCon}{How little variation between iterations will break the loop early calculated as an average per variable, assuming variables are between 0-1.}
  \item{standIn}{Should the data be locked between 0 and 1 before classification?}
  \item{distM}{Distance measure to calculate memberships: "euc" = euclidean distance "man" = Manhattan distance "eu2" = squared euclidean distance}
  \item{randRe}{If a class is empty at the end of an iteration, should it be re-assigned at random? Can disrupt existing clusters, but makes it more likely that all clusters are populated. Higher "its" value recommended.}
  \item{fileOut}{Name to write file to, defaults to temporary file.}
  \item{silent}{Should details of the classification be output as it works?}
  \item{interPlot}{Should the classification be plotted each iteration?}
}

\value{
  List containing two items:
  \item{Raster}{The classified raster file.}
  \item{Centres}{The centres used to classify the raster file.}
}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- KMeans(r, nCentres = 4)
r
}
