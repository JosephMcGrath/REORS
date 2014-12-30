\name{ExportKML}
\alias{ExportKML}

\title{Wrapper function to KML, }

\description{
Exports a single-layer RasterLayer into a KML format, for use in Google Earth.
}

\usage{
ExportKML(rasterIn, fileOut, colUse = "terrain", blurVal = 1)
}

\arguments{
  \item{rasterIn}{The RasterLayer to be exported, currently only supports single layer rasters, multi-layers are possible though.}
  \item{fileOut}{the name to be given to the output KML file.}
  \item{colUse}{Which colour palette to build, currently can be "rainbow", "heat", or "terrain". More colours to come later.}
  \item{blurVal}{The blur value to be passed directly to the KML function.}
}

\value{None, .kml file written to disk.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
ExportKML(r, tempfile())
}
