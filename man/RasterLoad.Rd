\name{RasterLoad}
\alias{RasterLoad}
\alias{RasterBrowse}

\title{
Versatile method for loading raster data into R.
}

\description{
Loads data from multiple, potentially different sources into a single object.
Designed to be inserted into other functions for simplicity, can have rasters with different grids by returning a list.\cr
RasterBrowse is a wrapper function, allowing for smoother loading of files, defaults to "stack" output.
}

\usage{
RasterLoad(dataIn, retForm = "list", fileOut = TempRasterName())
RasterBrowse(retForm = "stack", fileOut = TempRasterName())
}

\arguments{
  \item{dataIn}{dataIn: A list of the different files to be conglomerated. Accepted formats used: RasterLayer, RasterBrick RasterStack, Character vector, containing the names of files to be loaded, List, containing any of the above, or another nested list.}
  \item{retForm}{retForm: Format to return data in, may be:\cr
   "stack" for a RasterStack.\cr
   "brick" for a RasterBrick. May take a LONG time to process.\cr
   "list"  for a list of objects. Mainly for different extents ect (default)}
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
