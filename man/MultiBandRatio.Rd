\name{MulitBandRatio}
\alias{MulitBandRatio}

\title{
Calculates several band ratios simultaneously.
}

\description{
Calculates multiple band ratios in one pass, useful for cases where there are a large number of ratios per image.
}

\usage{
MultiBandRatio(rasterIn, bands, fileOut = REORStemp(), silent = TRUE)
}

\arguments{
  \item{rasterIn}{The multi-layered raster to calculate ratios from.}
  \item{bands}{A list of band ratios to calculate. First number in each pair is taken as the numerator, the second as the denominator.}
  \item{fileOut}{The name of the file to write out, defaults to a temporary file.}
  \item{silent}{Should the function work without progress reports?}
}

\value{A RasterBrick of the various ratios, in the order they were given at input.}

\author{Joseph McGrath}

\examples{
r <- brick(ncol = 50, nrow = 50, nl = 3)
values(r) <- sample(1:2500, 7500, TRUE)
r
r <- MulitBandRatio(r, list(c(1,2), c(3,1), c(2,3)))
r
}
