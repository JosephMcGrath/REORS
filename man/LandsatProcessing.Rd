\name{LandsatProcessing}
\alias{LandsatProcessing}

\title{
Semi-automated processing of Landsat imagery.
}

\description{
Pulls in Landsat bands from single layers and stacks them together. The function assumes that the files are in the default format downloaded from the USGS (based on the format used by the EarthExplorer website at the start of 2015). Also strongly suggested that no additional files are in the folder - though they will not be included unless they share the same properties (extent, resolution ect).
}

\usage{
LandsatProcessing(filePath, props = NULL, fileOut = paste0(c(filePath, "Processed"), collapse = "/"), silent = TRUE)
}

\arguments{
  \item{filePath}{The folder containing the bands to be combined.}
  \item{props}{Optional properties to modify the output to. Passes the argument to projectRaster. If a Raster* object, passes to the "to" argument, otherwise will be passed to the "crs" argument.}
  \item{fileOut}{The output location, including filename. Defaults to inside the folder files are taken from.}
  \item{silent}{should the function work without progress reports?}
}

\value{The processed RasterBrick. Also writes a text file describing bands.}

\author{Joseph McGrath}
