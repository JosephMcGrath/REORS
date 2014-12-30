\name{GaussianSmooth}
\alias{GaussianSmooth}

\title{
Gaussian smooth of a RasterLayer.
}

\description{
Uses a Gaussian weighting to smooth an image input. Currently only works on single-layer images.
}

\usage{
GaussianSmooth(rasterIn, kernelSize, outName = tempfile(), sig = 1, type = "circle")
}

\arguments{
  \item{rasterIn}{The raster file to be smoothed}
  \item{kernelSize}{The size of kernel to smooth using.}
  \item{outName}{the name to be given to the resulting file.}
  \item{sig}{the value of sigma used in the calculation of the filter. Higher values will give a more focused filter.}
  \item{type}{to be passed to WMat - the shape of kernel to use.}
}

\value{A RasterLayer containing the smoothed results.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- GaussianSmooth(r, 5)
r
plot(r)
}
