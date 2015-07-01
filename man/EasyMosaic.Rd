\name{EasyMosaic}
\alias{EasyMosaic}

\title{Simplified mosaicing process.}

\description{
Mosaics a variety of input Raster* objects, automatically removing or resampling problem inputs.
}

\usage{
EasyMosaic(dataIn = choose.files(), resample = FALSE, fileOut = TempRasterName(), silent = TRUE)
}

\arguments{
  \item{dataIn}A set of files to mosaic, defaults to choose.files(). Passed through RasterLoad.}
  \item{resample}{Boolean value - if any inputs have different resolutions ect. this will cause them to be re-sampled to the most common input. This can and will change data values, as well as take a lot of computation time. Generally un-advised but can be useful to produce an output from irregular inputs.}
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
r <- EasyMosaic() #And choose files
}
