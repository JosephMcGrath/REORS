\name{Standardise}
\alias{Standardise}

\title{
Locks the values of a Raster* object between a set maximum and minimum.
}

\description{
Takes all values in a given raster and scales them to be between a set minimum and maximum.
Useful to compare images with different maximum and minimum values. In many cases is a precursor to classification.
}

\usage{
Standardise(rasterIn, minMax = c(0, 1), intLock = FALSE, recalc = TRUE, fileOut = TempRasterName(), silent = TRUE)
}

\arguments{
  \item{rasterIn}{The raster file to be normalised.}
  \item{minMax}{The minimum/maximum values to stretch to as a vector.}
  \item{intLock}{Boolean; should it round to the nearest whole number?}
  \item{recalc}{should the minimum values be recalculated before running? Results may be inaccurate if not used, but takes extra time.}
  \item{fileOut}{The location to save the result, defaults to a temporary file.}
  \item{silent}{Should information about progress be returned?}
}

\value{A Raster* object with the results of normalisation, also saves a file output}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- Standardise(r)
r
}
