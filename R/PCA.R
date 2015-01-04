PCA <- function(rasterIn, npc = NULL, fileName = tempfile(), silent = TRUE){
#Calculates or estimates principal components for an input image.
#
#Currently not able to check if it works as intended. Would need commercial
# software for that. May add the option to sample the data to speed up the
# covariance matrix calculation.
#
#Args:
#  rasterIn: The raster file to perform principal component analysis on.
#  npc: The number of components to return. Defaults to the number of layers
#   of rasterIn.
#  fileName: Name of file to save to, defaults to a temporary file.
#  silent: should the function work without progress reports?
#Returns:
#  A list with two items:
#   -Raster: The image calculated from principal components.
#   -Eigens: A matrix containing the eigenvectors, eigenvalues & standard devs

  library("raster")
  
  cat("WARNING: PCA support is currently experimental, use with caution.\n")
  
  if(nlayers(rasterIn) < 2){
    stop("Input raster needs to have more than one layer.")
  }
  if(is.null(npc)) npc <- nlayers(rasterIn)
  if(npc > nlayers(rasterIn)){
    stop("Cannot calculate more components than input layers.\n")
  }
  blocks <- blockSize(rasterIn)
#Using a custom method as it's faster, bypasses temp file issues and allows
 #reporting as the function runs.
  
#--Calculate mean values------------------------------------------------------
  if(!silent) cat("Calculating mean values. (Step 1/3)\n")
  mVals <- rep(0, nlayers(rasterIn))
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    mVals <- mVals + colSums(tempValues, na.rm = TRUE)
    
  }
  
  mVals <- mVals / ncell(rasterIn)
  covMat <- matrix(0, ncol = nlayers(rasterIn), nrow = nlayers(rasterIn))
  
#--Calculate covariance matrix------------------------------------------------
  if(!silent) cat("Calculating covariance matrix. (Step 2/3)\n")
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
    rasterIn,
    row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
        
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
  }
  
  covMat <- covMat / (ncell(rasterIn) - 1)
  
#--Calculate eigenvalues & principal components-------------------------------
  eigens <- eigen(covMat)
  
  rasterTemp <- RasterShell(rasterIn, npc, fileName)
  rasterTemp <- writeStart(rasterTemp, filename = fileName, format = "GTiff",
   overwrite = TRUE)
  
  if(!silent) cat("Calculating PCA. (Step 3/3)\n")
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
    rasterIn,
    row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    tempValues <- tempValues %*% eigens[[2]]
    
    rasterTemp <- writeValues(
     x = rasterTemp,
     v = tempValues[, 1:npc],
     start = blocks$row[i]
    )
  }
  
  rasterTemp <- writeStop(rasterTemp)
  
  valMat <- rbind(
   eigens[[2]],
   eigens[[1]],
   eigens[[1]] / sum(eigens[[1]]) * 100,
   cumsum(eigens[[1]] / sum(eigens[[1]]) * 100)
  )
  
  colnames(valMat) <- sprintf("Band %s", 1:ncol(valMat))
  rownames(valMat) <- append(1:ncol(valMat), c("Eigenvectors",
   "Std Deviation", "cum std dev")
  )
  
  return(list("Raster" = rasterTemp, "Eigens" = valMat))
}
