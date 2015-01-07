\name{FocalAngle}
\alias{FocalAngle}

\title{
Calculates and summarises the vertical angle between the pixel and it's surroundings.
}

\description{
Calculates the vertical angle between the centre-points of a focal window and its other cells. The methodology used for this is similar to that used for topographic openness.
}

\usage{
FocalAngle(rasterIn, kernelSize, fileOut = tempfile(pattern = "REORS"))
}

\arguments{
  \item{rasterIn}{The raster file to use in calculation, passed through RasterLoad, though only the first layer is taken at the present time.}
  \item{kernelSize}{The size of kernel to use in focal calculations.}
  \item{fileOut}{Name to be used in output, defaults to a temporary file.}
}

\value{The RasterLayer containing calculated angles, also saved to disk.}

\references{
Yokoyama,R., Shirasawa,M., Pike,R.J. (2002)
  Visualizing topography by openness: a new application of image processing to digital elevation models. 
  Photogrammetric engineering and remote sensing, 68, 257-266.
}

\author{Joseph McGrath}

\note{The units that resolution are measured in must be the same as the vertical units. This most likely metres.}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- FocalAngle(r, 5)
r
plot(r)
}
