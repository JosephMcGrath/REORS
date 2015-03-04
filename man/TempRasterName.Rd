\name{REORStemp}
\alias{REORStemp}

\title{
Temporary file assignments.
}

\description{
Generates temporary file names using tempfile. Intended for use inside other functions, rather than being called independently.
}

\usage{
TempRasterName()
}

\value{Temporary file location.}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- DOS(r, fileOut = TempRasterName())
r
}
