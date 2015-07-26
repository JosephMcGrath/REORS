\name{SpectralProfiler}
\alias{SpectralProfiler}

\title{
Extracts data from a raster, 
}

\description{
Calculates the spectral properties of a set of shapes. For points, the profile for each point is given. For lines, the profile along the line is given. For polygons, the average value for pixels under the polygon.
}

\usage{
SpectralProfiler(rasterIn, shapeIn = "point", nProfile = 1, returnSpatial = FALSE, plotOut = TRUE, silent = TRUE)
}

\arguments{
  \item{rasterIn}{Raster* object to extract values from.}
  \item{shapeIn}{Either a Spatial* object, or one of the following strings:\cr
  "point": To draw points on the plot window.\cr
  "line": To draw lines on the plot window.\cr
  "poly": To draw polygons on the plot window.}
  \item{nProfile}{If creating points or polygons, how many should be created.}
  \item{returnSpatial}{If the spectral profiles should be returned as a SpatialDataFrame rather than a matrix.}
  \item{silent}{Should progress be printed to the console?}
}

\value{
Either a matrix of digital numbers for each point/line/polygon provided or a Spatial* object with the digital numbers attached. For points, values are collected per-pixel. For lines, each pixel along each line is returned in order (as a list of matrices). Due to the many-one relationship of values to lines, spatial results are returned as a list of shapes and values. For polygons, the average value for each polygon is returned.
}

\author{Joseph McGrath}

\examples{
\dontrun{
r <- brick(ncol = 50, nrow = 50, nl = 4)
values(r) <- sample(1:2500, 10000, TRUE)
r <- SpectralProfiler(r, 3)
r
}
}
