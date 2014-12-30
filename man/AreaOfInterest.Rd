\name{AreaOfInterest}
\alias{AreaOfInterest}

\title{
Area of interest selection.
}

\description{
Takes in a raster* object, then uses a spatial* object to crop it. If an object isn't provided, it can also take co-ordinates from the plot window. Can use a pre-existing plot window if requested too.
}

\usage{
AreaOfInterest(rasterIn, maskLayer = "box", plotNew = TRUE, fileName = tempfile())
}

\arguments{
  \item{rasterIn}{The raster file to be masked, passed through RasterIn}
  \item{maskLayer}{The raster or shapefile to be used, may also be special cases: "poly", To draw a polygon on the plot window to mask by. "box", To draw a box on the plot window to mask by}
  \item{plotNew}{plotNew: If drawing on the plot window, does a new plot need to be made?}
  \item{fileName}{fileName:  The name to save the file as, defaults to temporary file.}
}

\value{
A new raster* object with the modified extent.
}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- AreaOfInterest(r)
r
plot(r)
}
