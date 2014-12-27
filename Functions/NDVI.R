NDVI <- function(rasterIn, NIR, VIS, fileName = tempfile()){                  #<-- Merge with BandRatio? Create specialist band math function?
#A specialised form of band maths, may be replaced later with a more general
# function incorperating several common functions
#
#Args:
#  rasterIn: the multi-layered raster to use
#  NIR: the band of rasterIn that represents the near-infrared wavelength
#  VIS: the band of rasterIn that represents the visible red wavelength
#  fileName: the name of the file to write out, defaults to a temporary file
#Returns:
#  A rasterLayer of the resulting calculation
  
  library("raster")
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- raster(rasterIn)
  rasterTemp <- writeStart(rasterTemp, filename = fileName, format = "GTiff",
   overwrite = TRUE
  )
  
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
        (x[[NIR]] - x[[VIS]]) / (x[[NIR]] + x[[VIS]])
      }
     ),
     start = blocks$row[i]
    )
    
  }
    
  rasterTemp <- writeStop(rasterTemp)

  return(rasterTemp)
}
