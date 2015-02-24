\name{CMeans}
\alias{CMeans}

\title{
Applies fuzzy c-means classification to an Raster* object.
}

\description{
Uses the fuzzy c-means algorithm to attempt to classify the input image, with some additional customisation available. Iteratively assigns fuzzy membership values to pixels in an attempt to produce the best classification. The algorithm is heavily influenced by the initial cluster centres.
}

\usage{CMeans(rasterIn, nCentres = 10, its = 1, weight = 1, fuzz = 2, init = "lin",, breakCon = 0.01, standIn = FALSE, distM = "euc",
 fileOut = tempfile(pattern = "REORS - ") silent = TRUE, interPlot = FALSE)
}

\arguments{
  \item{rasterIn}{Raster objects to be classified. Ran through RasterLoad.}
  \item{nCentres}{Number of clusters to split the data into. Ignored if "init" is uses pre-set centres.}
  \item{its}{Maximum number of iterations to run the algorithm for.}
  \item{weight}{The weights to apply to each layer (higher weight means an greater importance to clustering, can be integers or decimals. Must be a single values or have length equal to the number of input layers.}
  \item{fuzz}{The fuzzification parameter (often called m), higher values means membership values are drawn from a more diverse range of pixels. Cannot be 1 by definition, values below 1 will produces weird results.}
  \item{init}{How should the clusters be initialised? Current methods are: "lin": Linearly, from the minimum value in each layer, to the maximum. "rand": Randomly within the minimum/maximum available values. Matrix of centres, one column for each centre, one row for each layer.}
  \item{breakCon}{How little variation between iterations will break the loop early calculated as an average per variable, assuming variables are between 0-1.}
  \item{standIn}{Should the data be locked between 0 and 1 before classification?}
  \item{distM}{Distance measure to calculate memberships: "euc" = euclidean distance "man" = Manhattan distance "eu2" = squared euclidean distance}
  \item{fileOut}{Name to write file to, defaults to temporary file.}
  \item{silent}{Should details of the classification be output as it works?}
  \item{interPlot}{Should the classification be plotted each iteration?}
}

\value{
List containing two items:
  \item{Raster}{The classified raster file.}
  \item{Centres}{The centres used to classify the raster file.}
}

\references{
Bezdek,J.C. (1981). Pattern recognition with fuzzy objective function algorithms. Kluwer Academic Publishers.
}

\author{Joseph McGrath}

\Notes{Weighing is currently handled as values are pulled in, probably less efficient overall, but is simpler this way for now. Might also add some flexibility on weighting, though that is somewhat tenuous.}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- CMeans(r, nCentres = 4)
r
}
