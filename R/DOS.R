DOS <- function(rasterIn, recalc = FALSE,
 fileOut = REORStemp(), silent = TRUE){
#Simple dark object subtraction for atmospheric correction of spectral images.
#Intended to improve calculations rather than visualisation.
#
#Requires: RasterLoad, RasterShell
#
#Args:
#  rasterIn: The raster to correct, passed through RasterLoad.
#  recalc: Should the minimum values be recalculated before running? Results
#   may be inaccurate if not used, but takes extra time.
#  fileOut: The name of the file to write out, defaults to a temporary file.
#  silent: Should the function work without progress reports?
#Returns:
#  A rasterLayer of the image after subtraction.
  
  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(recalc){
    if(!silent) cat("Calculating minimum and maximum values of input.\n")
    rasterIn <- setMinMax(rasterIn)
  }
  
  blocks <- blockSize(rasterIn)
  rasterOut <- RasterShell(rasterIn)
  
  rasterOut <- writeStart(rasterOut, filename = fileOut, format = "GTiff",
   overwrite = TRUE)
  minV <- minValue(rasterIn)
  
  if(!silent) cat("Applying simple dark object subtraction:\nWriting to %s\n")
  
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
     
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    rasterOut <- writeValues(
     x = rasterOut,
     v = t(t(tempValues) - minV),
     start = blocks$row[i]
    )
    
  }
    
  rasterOut <- writeStop(rasterOut)

  return(rasterOut)
}
