\name{DOS}
\alias{DOS}

\title{
Function to apply simple dark object subtraction to a spectral image.
}

\description{
Applies simple dark object subtraction to an image by subtracting minimum values from each band.
No benefit for the purpose of visualisation in most cases, as it's often applied when rendering in most pieces of software.
Probably better than nothing for further calculations however.
}

\usage{
DOS(rasterIn, fileName = tempfile(), silent = TRUE)
}

\arguments{
  \item{rasterIn}{the raster to correct, passed through RasterLoad}
  \item{fileName}{the name of the file to write out, defaults to a temporary file.}
  \item{silent}{should the function work without progress reports?}
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
