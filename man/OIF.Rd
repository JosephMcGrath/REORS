\name{OIF}
\alias{OIF}

\title{Calculates optimal index factors for a given raster.}

\description{
Calculates optimal index factors for a given raster.
}

\usage{
OIF(rasterIn, layersIn = 3, silent = TRUE)
}

\arguments{
  \item{rasterIn}{The raster file to perform principal component analysis on.}
  \item{layersIn}{The number of layers to be considered for each combination. defaults to 3, for RGB representation.}
  \item{silent}{Should the function work without progress reports?}
}

\details{
The optimal index factor attempts to list the combination of layers that provides the most unique information. This is done using the total standard deviation of each layer, divided by the total covatiance between the layers.
}

\value{
A matrix containing a list of layersIn number of layers, then the optimal index factor for that combination of layers.
}

\author{Joseph McGrath}

\references{The formula used here was taken from http://spatial-analyst.net/ILWIS/htm/ilwisapp/optimum_index_factor_functionality_algorithm.htm}

\examples{
r <- brick(ncol = 50, nrow = 50, nl = 7)
values(r) <- sample(1:2500, 17500, TRUE)

OIF(r)
}
