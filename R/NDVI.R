NDVI <- function(rasterIn, NIR, VIS, fileOut = TempRasterName(),
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
    rasterOut <- RasterShell(rasterIn, 1)
    rasterOut <- writeStart(rasterOut,
                            filename = fileOut,
                            format = "GTiff",
                            overwrite = TRUE
                            )
  
    if(!silent){
        cat("Calculating NDVI.\n")
    }
    for(i in 1:blocks$n){
        if(!silent){
            cat(sprintf("\tProcessing block %s of %s\t(%s percent)",
                        i,
                        blocks$n,
                        round(i / blocks$n * 100)
                        ))

    tempValues <- getValues(rasterIn,
                            row = blocks$row[i],
                            nrow = blocks$nrow[i]
                            )
    if(!silent){
        cat(".")
    }

    tempValues <- (tempValues[, NIR] - tempValues[, VIS]) /
                  (tempValues[, NIR] + tempValues[, VIS])
    if(!silent){
        cat(".")
    }

    rasterOut <- writeValues(x = rasterOut,
                             v = tempValues,
                             start = blocks$row[i]
                             )
    if(!silent){
        cat(".\n")
    }

    }

    rasterOut <- writeStop(rasterOut)

    return(rasterOut)
}
