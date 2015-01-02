PCA <- function(rasterIn, npc = nlayers(rasterIn), sampleSize = NULL,
 fileName = tempfile(), silent = TRUE){
#Calculates or estimates principal components for an input image.
#
#Currently not able to check if it works as intended. Would need commercial
# software for that. The full calculation takes a prohibitively long time
# (multiple hours) to calculate for large image. Bottleneck appears to be the
# calculation of correlation matrix.
#
#Args:
#  rasterIn: The raster file to perform principal component analysis on.
#  npc: The number of components to return. Defaults to the number of layers
#   of rasterIn.
#  sampleSize: The sample size used to estimate principal components, taken
#   randomly. If omitted will use entire image via layerStats.
#  fileName: Name of file to save to, defaults to a temporary file.
#  silent: should the function work without progress reports?
#Returns:
#  A RasterBrick containing the principal components.

  library("raster")
  
  cat("WARNING: PCA support is currently experimental, use with caution.\n")
  
  if(npc > nlayers(rasterIn)){
    stop("Cannot calculate more components than input layers.\n")
  }
  
  if(!silent) cat("Calculating eigen values & vectors.\n")
  
  if(is.null(sampleSize)){
    temp <- layerStats(rasterIn, "pearson")[[1]]
  } else if(is.numeric(sampleSize)){
    sampleSize <- min(sampleSize, ncell(rasterIn))
    temp <- cor(sampleRandom(rasterIn, sampleSize))
  } else stop("Invalid sample size given, must be absent or a number.\n")
  
  temp <- eigen(temp, TRUE)
  
  eigenRet <- rbind(
   temp[[2]],
   temp[[1]],
   temp[[1]] / sum(temp[[1]]) * 100,
   cumsum(temp[[1]] / sum(temp[[1]]) * 100)
  )
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- RasterShell(rasterIn, npc, fileName)
  rasterTemp <- writeStart(rasterTemp, filename = fileName, format = "GTiff",
   overwrite = TRUE)
  
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    tempValues <- tempValues %*% temp[[2]]
    
    rasterTemp <- writeValues(
     x = rasterTemp,
     v = tempValues[, 1:npc],
     start = blocks$row[i]
    )
    
  }
    
  rasterTemp <- writeStop(rasterTemp)
  
  #Normalisation step here
  
  return(list(rasterTemp, eigenRet))
}

PCA(rasterIn, sampleSize = 5000, silent = FALSE)
