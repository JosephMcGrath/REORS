\name{FocalSummary}
\alias{FocalSummary}

\title{
Calculates values in a focal window with some added functionality.
}

\description{
Calculates values using a circular kernel, rather than the standard square one. Additionally has several pre-set functions from the literature.
}

\usage{
FocalSummary(rasterIn, kernelSize, sumFun, outName = tempfile())
}

\arguments{
  \item{rasterIn}{The raster file to use in calculation, passed through RasterLoad, though only the first layer is taken at the present time.}
  \item{kernelSize}{The size of kernel to use}
  \item{sumFun}{A function used to summarise the kernel e.g. min, of max. Will only use the first result of a function if it returns are multiple. Special cases are input as strings: "range" for the difference between max and min values (e.g. for relief). "hyps" for hypsometric integral. "AMin" for the difference between centre and minimum of the window. "BMax" for the difference between maximum and centre of the window.}
  \item{outName}{Name to be used in output, defaults to temporary file.}
}

\value{A RasterLayer containing the summarised values.}

\references{
Kühni & Pfiffner (2001)
  The relief of the Swiss Alps and adjacent areas and its relation to lithology and structure: topographic analysis from a 250-m DEM.
  Geomorphology, 41, 285-307.
Pike & Wilson (1971)
  Elevation-relief ratio, hypsometric integral, and geomorphic area-altitude analysis.
  Geological Society of America Bulletin, 82, 1079-1084
}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- FocalSummary(r, 5, "range")
r
plot(r)
}
