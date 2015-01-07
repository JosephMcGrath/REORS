MulitBandRatio <- function(rasterIn, bands,
 fileOut = tempfile(pattern = "REORS"), silent = TRUE){
#Simultaneously calculates multiple band ratios for a multi-band raster input
#
#Requires: RasterLoad
#
#Args:
#  rasterIn: the multi-layered raster to use
#  bands: A list containing pairs of bands to take as ratios
#  fileOut: the name of the file to write out, defaults to a temporary file
#  silent: should the function work without progress reports?
#Returns:
#  A RasterBrick of the various ratios, in order.
  
  library("raster")
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(nlayers(rasterIn) < 2) stop("Input raster needs more than one layer.\n")
  for(i in 1:length(bands)){
    if(length(bands[[i]]) != 2){
      stop("Specified band pairs must be of length 2.\n")
    }
    if(!is.numeric(bands[[i]])){
      stop("Bands must be specified as numeric values.\n")
    }
  }
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- RasterShell(rasterIn, length(bands))
  rasterTemp <- writeStart(rasterTemp, filename = fileOut, format = "GTiff",
   overwrite = TRUE)
  
  if(!silent) cat("Calculating band ratios.\n")
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    temp <- c()
    for(j in 1:length(bands)){
      temp <- cbind(temp, tempValues[, bands[[j]][1]] / tempValues[, bands[[j]][2]])
    }
    
    rasterTemp <- writeValues(
     x = rasterTemp,
     v = temp,
     start = blocks$row[i]
    )
    
  }
    
  rasterTemp <- writeStop(rasterTemp)

  return(rasterTemp)
}
