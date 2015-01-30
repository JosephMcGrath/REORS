\name{CMeans}
\alias{CMeans}

\title{
Applies fuzzy c-means classification to an input Raster* object.
}

\description{
Uses the standard fuzzy c-means algorithm, with some additional customisation available. Still needs some optimisation.
}

\usage{
CMeans(rasterIn, nCentres = 10, its = 1, weight = 1, fuzz = 2, init = "lin", fileOut = tempfile(pattern = "REORS"), breakCon = 0.01, standIn = FALSE, distM = "euc", silent = TRUE, interPlot = FALSE)
}

\arguments{
  \item{rasterIn}{Name of the image file to classify. Maybe run it through a cleaning function first?}
  \item{nCentres}{How many clusters should the data be split into?}
  \item{its}{Maximum number of iterations to run the algorithm for.}
  \item{weight}{The weights to apply to each layer (higher weight means an greater importance to clustering, can be integers or decimals. Must be a single values or have length equal to the number of input layers.}
  \item{fuzz}{The fuzzification parameter (often called m), higher values means membership values are drawn from a more diverse range of pixels. Cannot be 1 by definition, values below 1 will produces weird results.}
  \item{init}{How should the clusters be initialised? Current methods are: "lin": Linearly, from the minimum value in each layer, to the maximum. "rand": Randomly within the minimum/maximum available values. Matrix of centres, one column for each centre, one row for each layer.}
  \item{fileOut}{Name to write file to, defaults to temporary file.}
  \item{breakCon}{How little variation between iterations will break the loop early calculated as an average per variable, assuming variables are between 0-1.}
  \item{standIn}{Should the data be locked between 0 and 1 before classification?}
  \item{distM}{Distance measure to calculate memberships: "euc" = euclidean distance "man" = Manhattan distance "eu2" = squared euclidean distance}
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

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- CMeans(r, nCentres = 4)
r
}
