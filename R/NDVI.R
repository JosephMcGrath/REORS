NDVI <- function(rasterIn, NIR, VIS, fileOut = tempfile(pattern = "REORS"),
 silent = TRUE){
#A specialised form of band maths, may be replaced later with a more general
# function incorporating several common functions.
#
#Requires: RasterLoad, RasterShell
#
#Args:
#  rasterIn: the multi-layered raster to use.
#  NIR: the band of rasterIn that represents the near-infrared wavelength.
#  VIS: the band of rasterIn that represents the visible red wavelength.
#  fileOut: the name of the file to write out, defaults to a temporary file.
#Returns:
#  A RasterLayer of NDVI values.
  
  library("raster")
  library("REORS")

 #rasterIn <- RasterLoad(rasterIn, retForm = "stack")

  blocks <- blockSize(rasterIn)
  rasterTemp <- RasterShell(rasterIn, 1)
  rasterTemp <- writeStart(rasterTemp, filename = fileOut, format = "GTiff",
   overwrite = TRUE
  )
  
  if(!silent) cat("Calculating NDVI.\n")
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )

    tempValues <- (tempValues[, NIR] - tempValues[, VIS]) /
    (tempValues[, NIR] + tempValues[, VIS])
    
    rasterTemp <- writeValues(
     x = rasterTemp,
     v = tempValues,
     start = blocks$row[i]
    )
    
  }
    
  rasterTemp <- writeStop(rasterTemp)

  return(rasterTemp)
}
