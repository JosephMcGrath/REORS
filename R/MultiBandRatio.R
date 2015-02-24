MultiBandRatio <- function(rasterIn, bands,
 fileOut = tempfile(pattern = "REORS - "), silent = TRUE){
#Calculates many band ratios in one pass, useful for cases where there are a
# large number of ratios per image.
#
#Requires: RasterLoad, RasterShell
#
#Args:
#  rasterIn: The multi-layered raster to calculate ratios from.
#  bands: A list of band ratios to calculate. First number in each pair is
#   taken as the numerator, the second as the denominator.
#  fileOut: The name of the file to write out, defaults to a temporary file.
#  silent: Should the function work without progress reports?
#Returns:
#  A RasterBrick of the various ratios, in the order they were given at input.
  
  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(nlayers(rasterIn) < 2) stop("Input raster needs more than one layer.\n")
  for(i in 1:length(bands)){
    if(length(bands[[i]]) != 2){
      stop("Specified band pairs must be of length 2.\n")
    }
    if(!is.numeric(bands[[i]])){
      stop("Bands must be specified as numeric values.\n")
    }
    if(max(bands[[i]]) > nlayers(rasterIn) | min(bands[[i]]) <= 1){
      stop("Ratios given reference bands that don't exist.\n")
    }
  }
  
  blocks <- blockSize(rasterIn)
  rasterOut <- RasterShell(rasterIn, length(bands))
  rasterOut <- writeStart(rasterOut, filename = fileOut, format = "GTiff",
   overwrite = TRUE)
  
  if(!silent) cat("Calculating band ratios.\nWriting to %s\n")
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
      temp <- cbind(temp, tempValues[, bands[[j]][1]] /
       tempValues[, bands[[j]][2]])
    }
    
    rasterOut <- writeValues(
     x = rasterOut,
     v = temp,
     start = blocks$row[i]
    )
    
  }
    
  rasterOut <- writeStop(rasterOut)

  return(rasterOut)
}
