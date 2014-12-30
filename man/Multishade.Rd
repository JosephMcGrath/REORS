\name{Multishade}
\alias{Multishade}

\title{
Calculates and stacks multiple hill-shaded images.
}

\description{
Calculates a hillshade for the red, green and blue channels. This allows for more nuanced displace of structures.
The default inputs for angles are fairly closely spaced, wide spacings often five results that are more difficult to interpret.
}

\usage{
Multishade(rasterIn, fileOut = NULL, angles = c(15, 20, 15), directions = c(100, 125, 150), autoPlot = TRUE)
}

\arguments{
  \item{rasterIn}{The raster file to be shaded - probably should be a DEM.}
  \item{fileOut}{The location to write to, if omitted is not written to memory.}
  \item{angles}{A vector of three azimuths of illumination, in RGB order.}
  \item{directions}{A vector of three directions of illumination, in RGB order.}
  \item{autoPlot}{Boolean - should the result be plotted on completion?}
}

\value{A rasterStack (RasterBrick if writing to file) of the result, all values stored as 8 bit integers.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- Multishade(r)
r
}
