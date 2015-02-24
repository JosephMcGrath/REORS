\name{FocalCalc}
\alias{FocalCalc}

\title{
Calculates focal summary of Raster* objects.
}

\description{
Takes the area surrounding a given Raster* object (one or several layers) and summarises them based on a used-defined, or pre-set function.

This function adds support for multi-layered Raster* objects, which at the time of writing are not supported by the raster package's "focal" function.
}

\usage{
FocalCalc(rasterIn, sumFun, kernelSize, kernelShape = "circle", na.rm = FALSE, fileOut = tempfile(pattern = "REORS - "), silent = TRUE)
}

\arguments{
  \item{rasterIn}{Raster objects to be classified. Ran through RasterLoad.}
  \item{sumFum}{The function used to summarise the focal values. Should return a single value (if multiple are returned, will use the first). Several pre-sets are available using strings:
   -"hyps": The hypsometric integral
   -"range": The difference between the maximum and minimum values in the kernel.
   -"wmean": Weighted mean, for when kernel values aren't equal.
   -"amin": The difference between the centre of the kernel and the minimum elevation in the kernel.
   -"bmax": The difference between the centre of the kernel and the maximum elevation in the kernel.}
  \item{kernelSize}{The size of the kernel to be used. May be either a single number (for a square matrix), a pair of numbers (for uneven height & width). Alternatively a pre-set matrix can be given (in which case kernelShape will be ignored), NA values can be given for pixels to be ignored. Note: kernel MUST have odd dimensions, can use NA values to simulate even sides.}
  \item{kernelShape}{The weighting/shape to give the kernel. The values in the kernel are multiplied by these - so take this into account when using non NA/1 values. The following options are available:
 -"square": All values in the kernel are set to 1
 -"circle": A circle of 1 values in the centre of the matrix, surrounded by NA values. With smaller kernels, the shape can be distorted.
 -"gaussian: Decreasing weighting outwards, based on Gaussian distribution. Sigma value for distribution is set to 1.}
  \item{na.rm}{Ignores NA values in the Raster* input.}
  \item{fileOut}{Name to be used in output, defaults to temporary file.}
  \item{silent}{Should details of the classification be output as it works?}
}

\value{
  A Raster* object with the same number of layers as input containing the calculated values.
}

\author{Joseph McGrath}

\examples{
r <- raster(ncol = 50, nrow = 50)
values(r) <- sample(1:2500, 2500, TRUE)
r
r <- FocalCalc(r, sum, 5)
r
}
