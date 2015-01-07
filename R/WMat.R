WMat <- function(matSize, type = "circle"){
#Creates a matrix of weights with the desired size and shape
#For use in focal calculations
#
#ToDo:
#  Option to weight the matrix (would merge the GaussianSmooth and
#   FocalSummary functions.
#
#Args:
#  matSize: The size to use for the edges of the matrix.
#  type: Shape to use for the matrix:
#   -"circle" an approximately circular matrix. (default)
#   -"square" for a standard square matrix.
#
#Returns:
#  A matrix of weights, either 1 or NA
  
  if(round(matSize) %% 2 == 0) stop("Cannot have even number or rows/columns.")
  if(matSize < 3) stop("Must have a weights matrix larger than 3.\n")
  
  ret <- matrix(rep(1, matSize ^ 2), ncol = matSize)
  
  if(type == "circle"){
    mid <- ceiling(matSize / 2)
    for(i in 1:matSize){
      for(j in 1:matSize){
	  if(round(sqrt((i - mid) ^ 2 + (j - mid) ^ 2)) > mid - 1){
	    ret[i, j] <- NA
	  }
	}
    }
  }
  
  return(ret)
}
