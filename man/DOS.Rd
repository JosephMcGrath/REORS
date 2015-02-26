\name{DOS}
\alias{DOS}

\title{
Simple dark object subtraction for atmospheric correction of spectral images.
}

\description{
Applies simple dark object subtraction to an image by subtracting minimum values from each band.
Intended to improve calculations rather than visualisation.
}

\usage{
DOS(rasterIn, recalc = FALSE, fileName = REORStemp(), silent = TRUE)
}

\arguments{
  \item{rasterIn}{The raster to correct, passed through RasterLoad.}
  \item{recalc}{Should the minimum values be recalculated before running? Results may be inaccurate if not used, but takes extra time.}
  \item{fileOut}{The name of the file to write out, defaults to a temporary file.}
  \item{silent}{Should the function work without progress reports?}
}

\value{A rasterLayer of the image after subtraction.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- DOS(r)
r
}
