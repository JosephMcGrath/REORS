AreaOfInterest <- function(rasterIn, maskLayer = "box",
 fileName = tempfile()){
#Function to use a polygon to mask out a certain area of a raster
#
#Requires: RasterLoad
#
#Args
#  rasterIn:  The raster file to be masked
#  maskLayer: The raster, shapefile to be used, may also be special cases:
#   -"poly": To draw a polygon on the plot window to mask by
#   -"box" : To draw a box on the plot window to mask by
#  fileName:  The name to save the file as, defaults to temporary file.
#
#Returns
#  Name of the written file

  library("raster")
  
#--If specified, draw the desired extent--------------------------------------
#Only plotting the first layer for simplicity here
  if(maskLayer == "poly"){
    plot(rasterIn, 1)
    maskLayer <- drawPoly()
  } else if(maskLayer == "box"){
    plot(rasterIn, 1)
    maskLayer <- drawExtent()
  }
  
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