\name{PCA}
\alias{PCA}

\title{Calculates or estimates principal components for an input image.}

\description{
Calculates or estimates principal components for an input image.
}

\usage{
function(rasterIn, npc = NULL, eigens = NULL, standOut = FALSE, fileOut = TempRasterName(), silent = TRUE)
}

\arguments{
  \item{rasterIn}{The raster file to perform principal component analysis on.}
  \item{npc}{The number of components to return. Defaults to the number of layers of rasterIn.}
  \item{standOut}{should the end result be 8 bit integer? (using the Standardise function).}
  \item{eigens}{Previously calculated eigenvalues to use instead of whole image. Not very transferable between images, but potentially useful to apply eigenvector from a subset of the whole image to the wider area.}
  \item{fileOut}{Name of file to save to, defaults to a temporary file.}
  \item{silent}{Should the function work without progress reports?}
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
