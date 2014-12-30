\name{RasterLoad}
\alias{RasterLoad}

\title{
Versatile method for loading raster data into R.
}

\description{
Loads data from multiple, potentially different sources into a single object.
Designed to be inserted into other functions for simplicity, can have rasters with different grids by returning a list.
}

\usage{
RasterLoad(dataIn, retForm = "list", fileOut = tempfile())
}

\arguments{
  \item{dataIn}{dataIn: A list of the different files to be conglomerated, formats used:
  -RasterLayer
  -RasterBrick
  -RasterStack
  -Character vector, containing the names of files to be loaded.
  -List, containing any of the above, or another list.}
  \item{retForm}{retForm: Format to return data in, may be:
  -"stack" for a RasterStack
  -"brick" for a RasterBrick. May take a LONG time to process
  -"list"  for a list of objects. Mainly for different extents ect (default)}
  \item{fileOut}{The location to save the output, if it is a RasterBrick.}
}

\value{Either a RasterStack, RasterBrick or list containing the input data.}

\author{Joseph McGrath}

\examples{
r1 <- raster(ncol = 50, nrow = 50)
r2 <- raster(ncol = 50, nrow = 50)
values(r1) <- sample(1:2500, 2500, TRUE)
values(r2) <- sample(1:2500, 2500, TRUE)
r <- RasterLoad(list(r1, r1, r2), retForm = "brick")
r
}
