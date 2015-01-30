DOS <- function(rasterIn, fileOut = tempfile(pattern = "REORS"),
 silent = TRUE){
#Applies simple dark object subtraction to an image.
#No benefit for the purpose of visualisation in most cases,
# as it's often applied when rendering.
#Probably better than nothing for further calculations however.
#Works by subtracting minimum values from each band.
#
#Requires: RasterLoad
#
#Args:
#  rasterIn: the raster to correct, passed through RasterLoad.
#  fileOut: the name of the file to write out, defaults to a temporary file.
#  silent: should the function work without progress reports?
#Returns:
#  A rasterLayer of the image after subtraction.
  
  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  blocks <- blockSize(rasterIn)
  rasterOut <- brick(rasterIn, values = FALSE)
  rasterOut <- writeStart(rasterOut, filename = fileOut, format = "GTiff",
   overwrite = TRUE)
  minV <- minValue(rasterIn)
  
  if(!silent) cat("Applying simple dark object subtraction:\n")
  
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
     
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    tempValues <- t(t(tempValues) - minV)
    
    rasterOut <- writeValues(
     x = rasterOut,
     v = tempValues,
     start = blocks$row[i]
    )
    
  }
    
  rasterOut <- writeStop(rasterOut)

  return(rasterOut)
}
