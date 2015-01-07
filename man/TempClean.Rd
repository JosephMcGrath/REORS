\name{BandRatio}
\alias{BandRatio}

\title{
Cleans the temporary directory of all files belonging to the REORS & raster packages.
}

\description{
As you might imagine, this cannot be undone, take care with this function. Searches for file beginning with "REORS" and ending in "tif", shouldn't overlap with anything.
}

\usage{
TempClean(tDir = tempdir())
}

\arguments{
  \item{tDir}{The folder to search and clear for temporary files.}
}

\value{None, deletes files from system.}

\author{Joseph McGrath}

\examples{
TempClean
}
