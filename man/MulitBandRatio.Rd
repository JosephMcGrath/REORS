\name{MulitBandRatio}
\alias{MulitBandRatio}

\title{
Calculates several band ratios simultaneously.
}

\description{
Calculates many band ratios in one pass, useful for cases where there are a large number of ratios per image.
}

\usage{
MulitBandRatio(rasterIn, bands, fileOut = tempfile(pattern = "REORS"), silent = TRUE)
}

\arguments{
  \item{rasterIn}{the multi-layered raster to use}
  \item{bands}{A list containing pairs of bands to take as ratios}
  \item{fileOut}{the name of the file to write out, defaults to a temporary file}
  \item{silent}{should the function work without progress reports?}
}

\value{A RasterBrick of the various ratios, in order.}

\author{Joseph McGrath}

\examples{
r <- brick(ncol = 50, nrow = 50, nl = 3)
values(r) <- sample(1:2500, 7500, TRUE)
r
r <- MulitBandRatio(r, list(c(1,2), c(3,1), c(2,3)))
r
}
