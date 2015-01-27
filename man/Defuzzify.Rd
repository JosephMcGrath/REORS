\name{Defuzzify}
\alias{Defuzzify}

\title{
Methods for converting fuzzy membership into crisp.
}

\description{
Applies simple dark object subtraction to an image by subtracting minimum values from each band.
No benefit for the purpose of visualisation in most cases, as it's often applied when rendering in most pieces of software.
Probably better than nothing for further calculations however.
}

\usage{
Defuzzify(rasterIn, meth = "max", opt = NULL, fileOut = tempfile(pattern = "REORS"), silent = TRUE)
}

\arguments{
  \item{rasterIn}{the raster to correct, passed through RasterLoad}
  \item{meth}{The method used for defuzzification, current options are: -"max": The class with the maximum membership. -"th" : The class with the maximum membership, so long as it's above a threshold (set with opt). Otherwise put into "undetermined" class.}
  \item{opt}{Options for the method chosen, for each meth argument: -"max": No options, argument is ignored. -"th" : The minimum membership value allowed.}
  \item{fileOut}{the name of the file to write out, defaults to a temporary file.}
  \item{silent}{should the function work without progress reports?}
}

\value{A rasterLayer of crisp membership values..}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- CMeans(r)
r
r <- Defuzzify(r)
r
}
