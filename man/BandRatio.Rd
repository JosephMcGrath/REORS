\name{BandRatio}
\alias{BandRatio}

\title{
Calculates band ratios of a multi-layer raster by dividing one band from another.
}

\description{
Divides one band by the other, giving the ratio, commonly used to isolate specific properties in a spectral image.
}

\usage{
BandRatio(rasterIn, band1, band2, fileOut = tempfile(pattern = "REORS"), silent = TRUE)
}

\arguments{
  \item{rasterIn}{The multi-layered raster to use, passed through RasterLoad.}
  \item{band1}{The band serving as the numerator.}
  \item{band2}{The band serving as the denominator.}
  \item{fileOut}{The name of the file to write out, defaults to a temporary file.}
  \item{silent}{Should the function work without progress reports?}
}

\value{A RasterLayer of the resulting calculation.}

\author{Joseph McGrath}

\examples{
r <- brick(ncol = 50, nrow = 50, nl = 2)
values(r) <- sample(1:2500, 5000, TRUE)
plot(r)
r <- BandRatio(r, 1, 2)
plot(r)
r
}
