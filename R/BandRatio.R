BandRatio <- function(rasterIn, band1, band2, fileName = tempfile()){
#Calculates band ratios of a multi-layer raster.
#
#Args:
#  rasterIn: the multi-layered raster to use
#  band1: the band serving as the numerator
#  band2: the band serving as the denominator
#  fileName: the name of the file to write out, defaults to a temporary file
#Returns:
#  A rasterLayer of the resulting calculation
  
  library("raster")
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- raster(rasterIn)
  rasterTemp <- writeStart(rasterTemp, filename = fileName, format = "GTiff",
   overwrite = TRUE)
  
  for(i in 1:blocks$n){
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    rasterTemp <- writeValues(
     x = rasterTemp,
     v = apply(
      X = tempValues,
      MARGIN = 1,
      FUN = function(x){
        x[[band1]] / x[[band2]]
      }
     ),
     start = blocks$row[i]
    )
    
  }
    
  rasterTemp <- writeStop(rasterTemp)

  return(rasterTemp)
}
