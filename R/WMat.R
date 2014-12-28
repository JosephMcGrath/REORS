WMat <- function(matSize, type = "circle"){
#Creates a matrix of weights with the desired size and shape
#For use in focal calculations
#
#Args:
#  matSize: the size to use for the edges of the matrix
#  type: shape to use for the matrix, square by default, also accepts circle
#
#Returns:
#  A matrix of weights, either 1 or NA
  
  library("raster")
  
  if(round(matSize) %% 2 == 0) stop("Cannot have even number or rows/columns")
  
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
