ExportRGB <- function(rasterIn, colPal = rainbow, colDepth = 512,
 fileOut = tempfile()){
#A function to write an image as an 8-bit RGB file, for non-GIS uses.
#
#Args:
#  rasterIn: The (single layer) image to be exported.
#  colPal: A function that builds a colour palette (in hex codes).
#  colDepth: The number of layers of colour to be saved.
#  fileOut: Where to save the end result, defaults to temporary.
#
#Returns:
#  An 8 bit RGB image, also saved to file.

  library("raster")
  colPal <- colPal(colDepth)
  rasterIn <- setMinMax(rasterIn)
  minMax <- c(minValue(rasterIn), maxValue(rasterIn) - minValue(rasterIn)
  )
  
  ret <- brick(rasterIn, nl = 3, values = FALSE)
  blocks <- blockSize(ret)
  
  ret <- writeStart(
   x = ret,
   filename = fileOut,
   format = "GTiff",
   overwrite = TRUE,
   datatype = "INT1U"
  )
  
  for(j in 1:blocks$n){
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[j],
     nrow = blocks$nrow[j]
    )
    
    tempValues <- t(col2rgb(colPal[round((tempValues - minMax[1]) / minMax[2] * 500)]))
    
    ret <- writeValues(
     x = ret,
     v = tempValues,
     start = blocks$row[j]
    )
  }
  
  ret <- writeStop(ret)
  
  return(ret)
}
