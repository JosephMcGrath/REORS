DOS <- function(rasterIn, fileName = tempfile(), silent = TRUE){
#Applies simple dark object subtraction to an image.
#No benefit for the purpose of visualisation in most cases,
# as it's often applied when rendering.
#Probably better than nothing for further calculations however.
#Works by subtracting minimum values from each band.
#
#Requires: RasterLoad
#
#Args:
#  rasterIn: the raster to correct, passed through RasterIn
#  fileName: the name of the file to write out, defaults to a temporary file
#Returns:
#  A rasterLayer of the resulting calculation
  
  library("raster")
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- brick(rasterIn, values = FALSE)
  rasterTemp <- writeStart(rasterTemp, filename = fileName, format = "GTiff",
   overwrite = TRUE)
  minV <- minValue(rasterIn)
  
  if(!silent) cat("Processing:\n")
  
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, round(blocks$n, i / blocks$n * 100)))
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    tempValues <- t(
     apply(
       X = tempValues,
       MARGIN = 1,
       FUN = function(x){
         x - minV
       }
      )
     )
    
    rasterTemp <- writeValues(
     x = rasterTemp,
     v = tempValues,
     start = blocks$row[i]
    )
    
  }
    
  rasterTemp <- writeStop(rasterTemp)

  return(rasterTemp)
}
