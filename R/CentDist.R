CentDist <- function(centres1, centres2){
#Calculates the distance between two sets of cluster centres.
#Currently only uses cluster centre proximity index (CCPI).
# more planned in the future.
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
