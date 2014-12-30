RasterShell <- function(rasterIn, layers = nlayers(rasterIn),
 fileOut = tempfile()){
#Creates a raster with the same metadata as the input raster, but with all the
# values as NA. Designed to provide an empty file to write to.
#Not 100% necessary, but may save time/errors later to have a single function.
#
#Args:
#  rasterIn: The raster object to take metadata values from.
#  layers: How many layers should the returned object have room for? Defaults
#   to the same as the input file.
#  fileOut: The directory to save the file to, defaults to a temporary file.  <--Not sure it works with an empty raster as there's no data to save.
#
#Returns:
#  A RasterLayer/RasterBrick (depending on the number of layers requested)
#   with no values attached.
  
  library("raster")

  if(layers == 1){
    ret <- raster(
     ext = extent(rasterIn),
     res = res(rasterIn),
     crs = crs(rasterIn),
     filename = fileOut,
     format = "GTiff",
     overwrite = TRUE
    )
  } else if(layers > 1){
    ret <- brick(
      x = rasterIn,
      values = FALSE,
      nl = layers,
      filename = fileOut,
      format = "GTiff",
      overwrite = TRUE
    )
  } else stop("Invalid number of layers defined")
  
  return(ret)
}
