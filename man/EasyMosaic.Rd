\name{EasyMosaic}
\alias{EasyMosaic}

\title{Simplified mosaicing process.}

\description{
Function to easily load, filter and mosaic a set of rasters with options to resolve differences in resolution, projection ect.
}

\usage{
EasyMosaic(rasterIn = RasterBrowse(), resolve = "", sumFun = mean, fileOut = TempRasterName(), silent = TRUE)
}

\arguments{
  \item{dataIn}{A set of files to mosaic, defaults to choose.files(). Passed through RasterLoad.}
  \item{resolve}{String to choose how to resolve any resolution differences:\cr
  "resample" - Resample all rasters to the most common input. Can take a significant quantity of time. Can be quite processor intensive too..\cr
  "multiple" - Produces a list of mosaiced rasters, one for each different resolution in. Multiple output rasters with sequential names are created.\cr
  All other option will return only the most common resolution. In the case of two being equally common, will pick the first in the input list.}
  \item{sumFun}{Function to be passed to mosaic function (for resolving duplicates)}
  \item{fileOut}{Name to write file to, defaults to temporary file.}
  \item{silent}{Should the function work without progress reports?}
}

\details{
Takes a variety of input Raster* objects and tests their compatibility before producing a mosaic of the results. As the resulting raster may only have a single resolution, inputs with different resolutions must be dealt with. The simplest method to do this is just dropping all but the most common resolutions, as this may produce unsatisfactory results the "resample" option forces R to resample each non-conforming input to the most common input. Where there are two resolutions occurring equally frequently - the resolution occurring in the earliest input is chosen.
}

\value{
A mosaic of all of the inputs, with non-conforming inputs either excluded  or resampled.
}

\author{Joseph McGrath}

\examples{
r <- EasyMosaic()
}
