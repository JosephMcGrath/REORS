\name{ExportRGB}
\alias{ExportRGB}

\title{
Exports a RasterLayer to a .tif file that can be read as a standard image.
}

\description{
The exported file is set up to be used in non-GIS/spatial programs. 
}

\usage{
ExportRGB(rasterIn, colPal = rainbow, colDepth = 512, fileOut = tempfile())
}

\arguments{
  \item{rasterIn}{The (single layer) image to be exported.}
  \item{colPal}{A function that builds a colour palette (in hex codes).}
  \item{colDepth}{The number of layers of colour to be saved.}
  \item{fileOut}{Where to save the end result, defaults to temporary.}
}

\value{An 8 bit RGB image, also saved to file.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
ExportRGB(r)
}
