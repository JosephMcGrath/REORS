AreaOfInterest <- function(rasterIn, maskLayer = "box",
 plotNew = TRUE, fileName = tempfile()){
#Function to use a polygon to mask out a certain area of a raster
#
#Requires: RasterLoad
#
#Args
#  rasterIn:  The raster file to be masked, passed through RasterIn
#  maskLayer: The raster or shapefile to be used, may also be special cases:
#   -"poly": To draw a polygon on the plot window to mask by
#   -"box" : To draw a box on the plot window to mask by
#  plotNew: If drawing on the plot window, does a new plot need to be made?
#  fileName:  The name to save the file as, defaults to temporary file.
#
#Returns
#  Name of the written file

  library("raster")
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
#--If specified, draw the desired extent--------------------------------------
#Only plotting the first layer for simplicity here
  if(maskLayer == "poly"){
    if(plotNew) plot(rasterIn, 1)
    maskLayerT <- drawPoly()
  } else if(maskLayer == "box"){
    if(plotNew) plot(rasterIn, 1)
    maskLayerT <- drawExtent()
  }
  maskLayer <- maskLayerT
  
#--Mask out the non-selected pixels-------------------------------------------
  if(class(maskLayer)[1] == "Extent"){
    ret <- crop(
     x= rasterIn,
     y = maskLayer,
     filename = fileName,
     format = "GTiff",
     overwrite = TRUE
    )
  } else {
    ret <- mask(
     x= rasterIn,
     mask = maskLayer,
     filename = fileName,
     format = "GTiff",
     overwrite = TRUE
    )
  }
  
  return(ret)
}
