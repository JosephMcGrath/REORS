\name{Defuzzify}
\alias{Defuzzify}

\title{
Methods for converting fuzzy membership into crisp.
}

\description{
Converts fuzzy memberships RasterBrick into single-layer crisp memberships. Two current options are to assign the pixel to it's highest membership or to assign the pixel to it's highest membership unless it's below a user-set threshold, in which case it is assigned to an "undecided" class.
}

\usage{
Defuzzify(rasterIn, meth = "max", opt = NULL, fileOut = TempRasterName(), silent = TRUE)
}

\arguments{
  \item{rasterIn}{The raster* object containing fuzzy membership values, should have one layer for each class. May also be the list of membership values and centroids from the CMeans function.}
  \item{meth}{The method used for defuzzification, current options are:\cr
   "max": The class with the maximum membership.\cr
   "th" : The class with the maximum membership, so long as it's above a threshold (set with opt). Otherwise put into "undetermined" class.}
  \item{opt}{Options for the method chosen, for each meth argument:\cr
   "max": No options, argument is ignored.\cr
   "th" : The minimum membership value allowed.}
  \item{fileOut}{the name of the file to write out, defaults to a temporary file.}
  \item{silent}{should the function work without progress reports?}
}

\value{A single-layer raster of crisp memberships, also written to disk.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- CMeans(r)[[1]]
r
r <- Defuzzify(r)
r
}
