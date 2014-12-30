\name{CMat}
\alias{CMat}

\title{
Calculates a confusion matrix for a classified image.
}

\description{
Function to calculate the confusion matrix between two raster layers. Calculates a wide variety of accuracy measures.
Added functionality with unsupervised classifications to link up best combinations of classes.
}

\usage{
CMat(classed, refernce, retT = "", reOrg = FALSE, stand = TRUE)
}

\arguments{
  \item{classed}{RasterLayer, or name of raster containing hard clustered values.}
  \item{refernce}{RasterLayer or name of raster containing reference values.}
  \item{retT}{One of several methods of data to return: "brief" will return kappa and accuracy values only. "order" will return the sequence the classes were reorganised into. All other values will return the full set of data.}
  \item{reOrg}{If the values should be reorganised based on how well the classes fit the reference values. Mainly useful for unsupervised classifications  where classes are assigned in a random/meaningless order.}
  \item{stand}{If the confusion matrix should be standardised (locked between 0 and 1). Makes it more comparable between classifications with different numbers of pixels.}
}

\value{
A list of values that describes the accuracy of the classification, depending on the inputs chosen can be one of the following:
  \item{adjusted}{confusion matrix after any reorganisation or standardisation}
  \item{kappaValue}{the kappa value for the confusion matrix}
  \item{producersAccuracy}{the produces accuracy for the matrix}
  \item{usersAccuracy}{the users accuracy for the confusion matrix}
  \item{overallAccuracy}{the overall accuracy value for the confusion matrix}
  \item{reorganising}{the order used to reorganise the clusters}
  \item{unadjusted}{the confusion matrix as it was originally calculated}
}

\references{
Congalton R.G. 1991 
 A review of assessing the accuracy of classifications of remotely sensed
 data. Remote sensing of environment. Volume 37, p35-46.
Pontius R.G. Millones M. 2011
 Death to Kappa: birth of quantity disagreement and allocation disagreement
 for accuracy assessment. International Journal of Remote Sensing.
 Volume 32, p4407-4429.
}

\author{Joseph McGrath}

\note{RasterLayer objects must be same resolution to work. May work with NA values in one layer, not thoroughly tested.}

\examples{
r1 <- raster(ncol = 50, nrow = 50)
r2 <- raster(ncol = 50, nrow = 50)
values(r1) <- sample(1:5, 2500, TRUE)
values(r2) <- sample(1:5, 2500, TRUE)
}
