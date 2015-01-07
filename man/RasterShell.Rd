\name{RasterShell}
\alias{RasterShell}

\title{
Creates a blank copy of a raster to fill with data.
}

\description{
Creates a raster with the same metadata as the input raster, but with all the values as NA.
Designed to provide an empty file to write to inside other functions.
}

\usage{
RasterShell(rasterIn, layers = nlayers(rasterIn))
}

\arguments{
  \item{rasterIn}{The raster object to take metadata values from.}
  \item{layers}{How many layers should the returned object have room for? Defaults to the same as the input file.}
}

\value{A RasterLayer/RasterBrick (depending on the number of layers requested) with no values attached.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- RasterShell(r)
r
}
