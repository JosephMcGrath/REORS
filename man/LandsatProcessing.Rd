\name{LandsatProcessing}
\alias{LandsatProcessing}

\title{
Automatic importing & processing of Landsat imagery.
}

\description{
Takes a Landsat file from just unzipped to stacked and masked.
}

\usage{
LandsatProcessing(filePath, LsType = NA, crsUse = NULL, silent = TRUE)
}

\arguments{
  \item{filePath}{The folder containing the bands to be combined Can go several folders deep, but will be return odd file names.}
  \item{LsType}{The number of the landsat sattelite being processed, main importance is fixing the band order of LS8 due to autosorting issues.}
  \item{crsUse}{Optional input for coordinate reference system to project the output into, if omitted, no projection is used.}
  \item{silent}{should the function work without progress reports?}
}

\value{None, files are written to disk in working directory, file names will be the same as folder containing the files.}

\author{Joseph McGrath}
