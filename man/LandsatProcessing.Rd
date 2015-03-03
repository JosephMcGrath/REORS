\name{LandsatProcessing}
\alias{LandsatProcessing}

\title{
Semi-automated processing of Landsat imagery.
}

\description{
Pulls in Landsat bands from single layers and stacks them together. The function assumes that the files are the only files in the set folder. Also assumes default naming scheme.
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
