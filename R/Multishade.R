Multishade <- function(rasterIn,  angles = c(15, 20, 15),
                       directions = c(100, 125, 150), autoPlot = TRUE,
                       fileOut = TempRasterName()){
#Function to calculate a hill-shade from multiple angles simultaneously
#
#Requires: RasterLoad
#
#Args:
#  rasterIn: The raster file to be shaded - probably should be a DEM.
#  fileOut: The location to write to, if omitted is not written to memory.
#  angle: A vector of three azimuths of illumination, in RGB order.
#  directions: A vector of three directions of illumination, in RGB order.
#  autoPlot: Boolean - should the result be plotted on completion?
#
#Returns:
#  A rasterStack (RasterBrick if writing to file) of the result, all
#   values stored as 8 bit integers.

    library("raster")
    rasterIn <- RasterLoad(rasterIn, retForm = "stack")
    rasterIn <- raster(rasterIn, layer = 1)

    checkVal <- FALSE

    if (length(angles) != 3){
        checkVal <- TRUE
    }
    if (length(directions) != 3){
        checkVal <- TRUE
    }
    if (nlayers(rasterIn) > 1){
        checkVal <- TRUE
    }

    if (checkVal){
        stop("Invalid inputs.")
    }

    asp <- terrain(x = rasterIn,
                   opt = "aspect"
                   )

    slp <- terrain(x = rasterIn,
                   opt = "slope"
                   )

    rasterOut <- stack(hillShade(slope = slp,
                                 aspect = asp,
                                 normalize = TRUE,
                                 angle = angles[1],
                                 direction = directions[1]
                                 ),
                       hillShade(slope = slp,
                                 aspect = asp,
                                 normalize = TRUE,
                                 angle = angles[2],
                                 direction = directions[2]
                                 ),
                       hillShade(slope = slp,
                                 aspect = asp,
                                 normalize = TRUE,
                                 angle = angles[3],
                                 direction = directions[3]
                                 )
                       )
    if(!is.null(fileOut)){
        brick(rasterOut,
             filename = fileOut,
             format = "GTiff",
             dataType = "INT1U",
             overwrite = TRUE
             )
    }

    if(autoPlot){
        plotRGB(rasterOut, stretch = "lin")
    }

    return(rasterOut)
}
