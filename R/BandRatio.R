BandRatio <- function(rasterIn, band1, band2,
 fileOut = REORStemp(), silent = TRUE){
#Calculates a band ratio from a multi-layer raster. 
#
#Requires: RasterLoad, RasterShell
#
#Args:
#  rasterIn: the multi-layered raster to use, passed through RasterLoad
#  band1: the band serving as the numerator
#  band2: the band serving as the denominator
#  fileOut: the name of the file to write out, defaults to a temporary file
#  silent: should the function work without progress reports?
#Returns:
#  A RasterLayer of the resulting calculation, also saved to disk.
  
  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(nlayers(rasterIn) < 2) stop("Input raster needs more than one layer.\n")
  if(!is.numeric(band1) | !is.numeric(band2)){
    stop("Bands must be specified as numeric values.\n")
  }
  if(nlayers(rasterIn) < max(band1, band2) | min(band1, band2) <= 0){
    stop("Band(s) specified do not exist.\n")
  }
  
  rasterOut <- RasterShell(rasterIn, 1)
  blocks <- blockSize(rasterIn)
  
  rasterOut <- writeStart(rasterOut, filename = fileOut, format = "GTiff",
   overwrite = TRUE)
  
  if(!silent) cat("Calculating band ratio:\nWriting to %s\n")
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
     v = tempValues[, band1] / tempValues[, band2],
     start = blocks$row[i]
    )
    
  }
    
  rasterOut <- writeStop(rasterOut)

  return(rasterOut)
}
