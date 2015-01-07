ExportKML <- function(rasterIn, fileOut, colUse = "terrain", blurVal = 1){
#Wrapper function for the KML function, very little added functionality.
# Plans to add some more useful features later.
#
#To do notes:                                                                 <--Added functionality, not sure how practical it is to implement.
#-Export RGB images to KML
#
#Args:
#  rasterIn: the RasterLayer to be exported, currently only supports
#   single layer rasters, multi-layers are possible though.
#  fileOut: the name to be given to the output KML file.
#  colUse: Which colour palette to build, currently can be "rainbow", "heat",
#   or "terrain". More colours to come later.
#  blurVal: The blur value to be passed directly to the KML function.
#
#Returns:
#  None, file output to system.

  library("raster")
  
  if(class(rasterIn) == "character") rasterIn <- raster(rasterIn)
  if(class(rasterIn)[1] != "RasterLayer") stop("Invalid input!")
  
  #Build a colour palette
  if(colUse == "rainbow"){
    colUse <- rainbow(255)
  } else if(colUse == "heat"){
    colUse <- heat.colors(255)
  } else if(colUse == "terrain"){
    colUse <- terrain.colors(255)
  }
  
  KML(
   x = rasterIn,
   filename = fileOut,
   col = colUse,
   blur = blurVal,
   overwrite = TRUE
  )

}
