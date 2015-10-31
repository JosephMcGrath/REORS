Defuzzify <- function(rasterIn, meth = "max", opt = NULL,
                      fileOut = TempRasterName(), silent = TRUE){
#Converts fuzzy memberships RasterBrick into single-layer crisp memberships.
#Two current options are to assign the pixel to it's highest membership or to
# assign the pixel to it's highest membership unless it's below a user-set
# threshold, in which case it is assigned to an "undecided" class.
#
#Requires: RasterShell
#
#Args:
#  rasterIn: The raster* object containing fuzzy membership values, should
#   have one layer for each class. May also be the list of membership values
#   and centroids from the CMeans function.
#  meth: The method used for defuzzification, current options are:
#   -"max": The class with the maximum membership.
#   -"th" : The class with the maximum membership, so long as it's above a
#     threshold (set with opt). Otherwise put into "undetermined" class.
#  opt: Options for the method chosen, for each meth argument:
#   -"max": No options, argument is ignored.
#   -"th" : The minimum membership value allowed.
#  fileOut: Name to write file to, defaults to temporary file.
#  silent: Should details of the classification be output as it works?
#
#Returns:
#  A single-layer raster of crisp memberships, also written to disk.

    library("raster")
    library("REORS")

    if(class(rasterIn) == "list" & class(rasterIn[[1]])[1] == "RasterBrick"){
        rasterIn <- rasterIn[[1]]
    }

    if(meth == "th" & !is.null(opt)){
        stop("Minimum threshold must be set with \"opt\".\n")
    }

    blocks <- blockSize(rasterIn)
    rasterOut <- RasterShell(rasterIn, 1)

    rasterOut <- writeStart(rasterOut,
                            filename = fileOut,
                            format = "GTiff",
                            overwrite = TRUE
                            )

    if(!silent){
        cat(sprintf("Defuzzifying raster:\nWriting to %s\n", fileOut))
    }

#--Take whichever membership value is highest---------------------------------
    if (meth == "max"){
        for (i in 1:blocks$n){
        if (!silent){
            cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
                        i, blocks$n, round(i / blocks$n * 100)
                        )
                )
        }

        tempValue <- getValues(rasterIn,
                               row = blocks$row[i],
                               nrow = blocks$nrow[i]
                               )

        crispMemb <- rep(NA, nrow(tempValue))

        for (j in 1:nrow(tempValue)){
            if (all(!is.na(tempValue[j, ]))){
                crispMemb[j] <- order(tempValue[j, ], decreasing = TRUE)[1]
            }
        }

        rasterOut <- writeValues(x = rasterOut,
                                 v = crispMemb,
                                 start = blocks$row[i]
                                 )
        }
    }

#--Require membership values to be above a threshold--------------------------
    if(meth == "th"){
        unDet <- nlayers(rasterIn) + 1

        for (i in 1:blocks$n){
        if (!silent){
            cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
                        i, blocks$n, round(i / blocks$n * 100)
                        )
                )
        }

        tempValue <- getValues(rasterIn,
                               row = blocks$row[i],
                               nrow = blocks$nrow[i]
                               )

        crispMemb <- rep(NA, nrow(tempValue))

        for (j in 1:nrow(tempValue)){
            if (all(!is.na(tempValue[j, ]))){
                crispMemb[j] <- order(tempValue[j, ], decreasing = TRUE)[1]
                if (max(tempValue[j, ]) < opt){
                    crispMemb[j] <- unDet
                }
            }
        }

        rasterOut <- writeValues(x = rasterOut,
                                 v = crispMemb,
                                 start = blocks$row[i]
                                 )
        }
    }

    rasterOut <- writeStop(rasterOut)

    return(rasterOut)
}
