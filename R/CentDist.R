CentDist <- function(centres1, centres2){
#Calculates the distance between two sets of cluster centres.
#Currently only uses cluster centre proximity index (CCPI).
# more planned in the future.
#
#Source for cluster centre proximity index.
#  Capitaine H.L., Frelicot C. (2011)
#  A fast fuzzy c-means algorithm for color image segmentation
#  Proceedings of European Society for Fuzzy Logic and Technology (EUSFLAT'2011) 1074-1081.
#
#Args:
#  centres1: The first set of cluster centres, with columns for centres and
#   rows for input layers.
#  centres2: The second set of centres, order does not matter but must have
#   the same dimensions at centres1
#
#Returns:
#  The distance measure between the two cluster centres.

    library("raster")

    if(any(dim(centres1) != dim(centres2))){
        stop("Centre matrices must have the same dimensions.\n")
    }

    ret <- sum(abs((centres2 - centres1) / centres2) /
           (ncol(centres1) * nrow(centres2)))

    return(ret)
}
