\name{Pansharpen}
\alias{Pansharpen}

\title{
Apply pan-sharpening to a multi-spectral image.
}

\description{
Applies pan-sharpening to a multispectral image to increase the apparent resolution of the input. Care should be taken that the panchromatic band is relevant to the multispectral bands being sharpened.
}

\usage{
Pansharpen(multiIn, panIn, bands, meth = "brovey", fileOut = TempRasterName(), silent = TRUE)
}

\arguments{
  \item{multiIn}{The multispectral image to be sharpened.}
  \item{panIn}{The higher resolution panchromatic image, the properties of this determines properties of the output.}
  \item{bands}{The bands of the multispectral image to use. Will be re-ordered into the given sequence.}
  \item{meth}{The method to use for pansharpening:\cr
   - "brovey": normalises the input based on total values for each pixel see references for details.}
  \item{fileOut}{The name of the file to write out, defaults to a temporary file.}
  \item{silent}{Should the function work without progress reports?}
}

\value{A RasterBrick of the pansharpened image. Note this image is unsuitable for most forms of further processing as values have been altered away from true reflectances.}

\references{
Scarp G. 2014. Spectral and spatial quality analysis of pan-sharpening algorithms: A case study in Istanbul.
 European Journal of Remote Sensing 47. 19-28.
}

\author{Joseph McGrath}

\examples{
r1 <- brick(ncol = 50, nrow = 50, nl = 3)
r2 <- raster(ncol = 100, nrow = 100, ext = extent(r1))
values(r1) <- sample(1:2500, 7500, TRUE)
values(r2) <- sample(1:2500, 10000, TRUE)
r <- Pansharpen(r1, r2, bands = c(3, 2, 1))
r
}
