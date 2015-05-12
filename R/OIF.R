OIF <- function(rasterIn, layersIn = 3, silent = TRUE){
#Calculates optimal index factors for a given raster.
#This should return the layers that contain the least shared information.
#
#Args:
#  rasterIn: Raster objects to be classified. Ran through RasterLoad.
#  layersIn: The number of layers to be considered for each combination.
#   defaults to 3, for RGB representation.
#  silent: Should details of the classification be output as it works?
#
#Returns:
#  A matrix 
#
#Formula from:
#http://spatial-analyst.net/ILWIS/htm/ilwisapp/optimum_index_factor_functionality_algorithm.htm
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(nlayers(rasterIn) <= layersIn){
    stop("layersIn must be smaller than number of layers provided.\n")
  }
  
  if(!silent) cat("Calculating optimal index factor.\n")
  
  blocks <- blockSize(rasterIn)

#--Calculate mean values------------------------------------------------------
  if(!silent) cat("\tCalculating mean values. (Step 1/3)\n")
  mVals <- rep(0, nlayers(rasterIn))
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\t\tProcessing block %s of %s\t(%s percent)",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    if(!silent) cat(".")
    
    mVals <- mVals + colSums(tempValues, na.rm = TRUE)
    if(!silent) cat(".\n")
    
  }
  
  mVals <- mVals / ncell(rasterIn)
  covMat <- matrix(0, ncol = nlayers(rasterIn), nrow = nlayers(rasterIn))
  
#--Calculate covariance matrix------------------------------------------------
  if(!silent) cat("\tCalculating covariance matrix. (Step 2/3)\n")
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\t\tProcessing block %s of %s\t(%s percent)",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    if(!silent) cat(".")
    
    tempValues <- t(t(tempValues) - mVals)
    
    for(j in 1:nlayers(rasterIn)){
      for(k in 1:nlayers(rasterIn)){
        if(j <= k){
          temp <- sum(tempValues[, j] * tempValues[, k], na.rm = TRUE)
          covMat[j, k] <- covMat[j, k] + temp
          covMat[k, j] <- covMat[j, k]
        }
      }
    }
    if(!silent) cat(".\n")
  }
  
  covMat <- covMat / (ncell(rasterIn))
  
#--Calculate OIF for each combination of layers-------------------------------
    if(!silent) cat("\tCalculating OIF. (Step 3/3)\n")
  combinations <- combn(1:nlayers(rasterIn), layersIn)
  stdevs <- sqrt(diag(covMat))
  
  ret <- matrix(nrow = ncol(combinations), ncol = layersIn + 1)
  for(i in 1:ncol(combinations)){
    temp1 <- sum(stdevs[combinations[,i]])
    temp2 <- 0
    temp3 <- combn(combinations[,i], 2)
    for(j in ncol(temp3)){
      temp2 <- temp2 + covMat[temp3[1, j], temp3[2, j]]
    }
    ret[i, ] <- append(names(rasterIn)[combinations[, i]], temp1 / temp2)
  }
  ret <- ret[order(ret[, ncol(ret)], decreasing = TRUE), ]
  
  return(ret)
}
