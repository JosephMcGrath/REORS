\name{PCA}
\alias{PCA}

\title{Calculates or estimates principal components for an input image.}

\description{
Calculates or estimates principal components for an input image.
}

\usage{
function(rasterIn, npc = nlayers(rasterIn), sampleSize = NULL, fileName = tempfile(), silent = TRUE)
}

\arguments{
  \item{rasterIn}{The raster file to perform principal component analysis on.}
  \item{npc}{The number of components to return. Defaults to the number of layers of rasterIn.}
  \item{fileName}{Name of file to save to, defaults to a temporary file.}
  \item{silent}{should the function work without progress reports?}
}

\details{
As the calculation of a correlation matrix can take a long time (~4 hours when testing with a full Landsat scene), functionality to take a random sample and estimate principal components from there has been added. Transformation of image to PCA takes the same duration after that.
}

\value{
A matrix of two items
  \item{Raster}{The image calculated from principal components.}
  \item{Eigens}{A matrix containing the eigenvectors, eigenvalues & standard deviations.}
}

\author{Joseph McGrath}

\note{Currently not able to check if it works as intended. Would need commercial software for that. The full calculation takes a prohibitively long time (multiple hours) to calculate for large image. Bottleneck appears to be the calculation of correlation matrix. The results will need standardising for most purposes.}

\examples{
r <- brick(ncol = 50, nrow = 50, nl = 2)
values(r) <- sample(1:2500, 5000, TRUE)
plot(r)
r <- PCA(r)
plot(r)
r
}
