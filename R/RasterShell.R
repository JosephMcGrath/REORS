RasterShell <- function(rasterIn, layers = nlayers(rasterIn)){
#Creates a raster with the same metadata as the input raster, but with no 
# values attached to the file.
#Not 100% necessary, but may save time/errors later to have a single function.
#Does not take a file connection as this doesn't work for RasterLayers, also
# no data is present. Take care when writing to the result of this function.
#
#Args:
#  rasterIn: The raster object to take metadata values from.
#  layers: How many layers should the returned object have room for? Defaults
#   to the same as the input file.
#
#Returns:
#  A RasterLayer/RasterBrick (depending on the number of layers requested)
#   with no values attached.

    library("raster")

    if(layers == 1){
        ret <- raster(ext = extent(rasterIn),
                      res = res(rasterIn),
                      crs = crs(rasterIn)
                      )
    } else if(layers > 1){
        ret <- brick(x = rasterIn,
                     values = FALSE,
                     nl = layers
                     )
    } else {
        stop("Invalid number of layers defined")
    }

    return(ret)
}
