\name{NDVI}
\alias{NDVI}

\title{
Calculates NDVI from a spectral image.
}

\description{
A specialised form of band maths, intended to detect/highlight vegetation.
}

\usage{
NDVI(rasterIn, NIR, VIS, fileOut = tempfile(pattern = "REORS"), silent = TRUE)
}

\arguments{
  \item{rasterIn}{The multi-layered raster to use.}
  \item{NIR}{the band of rasterIn that represents the near-infrared wavelength.}
  \item{VIS}{the band of rasterIn that represents the visible red wavelength.}
  \item{fileOut}{the name of the file to write out, defaults to a temporary file.}
  \item{silent}{should the function work without progress reports?}
}

\value{A RasterLayer of NDVI values.}

\author{Joseph McGrath}

\examples{
r <- brick(ncol = 50, nrow = 50, nl = 2)
values(r) <- sample(1:2500, 5000, TRUE)
plot(r)
r <- NDVI(r, 1, 2)
plot(r)
r
}
