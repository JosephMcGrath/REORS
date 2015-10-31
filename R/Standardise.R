Standardise <- function(rasterIn, minMax = c(0, 1), intLock = FALSE,
 recalc = TRUE, fileOut = TempRasterName(), silent = TRUE){
#Takes all values in a given raster and scales them to be between a set
# minimum and maximum.
#
#Args:
#  rasterIn: The raster file to be normalised.
#  minMax: The minimum/maximum values to stretch to as a vector.
#  intLock: Boolean; should it round to the nearest whole number?
#  recalc: should the minimum values be recalculated before running? Results
#   may be inaccurate if not used, but takes extra time.
#  fileOut: The location to save the result, defaults to a temporary file.
#  silent: Should information about progress be returned?
#
#Returns:
#  A Raster* object with the results of normalisation, also saves a file output

    library("raster")
    library("REORS")

#--Set up and detect data type to use-------------------------------------------

    rasterIn <- RasterLoad(rasterIn, "stack")  

    if (!silent){
        cat(sprintf(paste0("Standardising input between %s and %s.",
                           "%socking to whole numbers\n"
                           ),
                    minMax[1], minMax[2], if(intLock) "L" else "Not l"
                    )
            )
    }

    #May be quicker to impliment my own method?                                 ToDo
    if (recalc){
        if (!silent){
            cat("\tCalculating minimum and maximum values of input.\n")
        }
        rasterIn <- setMinMax(rasterIn)
    }

    mv <- list(minValue(rasterIn), maxValue(rasterIn),
               maxValue(rasterIn) - minValue(rasterIn)
               )

    ret <- RasterShell(rasterIn)
    blocks <- blockSize(rasterIn)

    if (intLock){
        dataTypeD <- sprintf("INT%s%s",
                             max(c(ceiling(log(max(abs(minMax)), base = 2)/ 8),
                                   1
                                   )
                                 ),
                             if(minMax[2] < 0){
                                 "S" else "U"
                             }
                             )
    } else {
        dataTypeD <- "FLT8S"
    }

    ret <- writeStart(x = ret,
                      filename = fileOut,
                      format = "GTiff",
                      datatype = dataTypeD,
                      overwrite = TRUE
                      )

#--Perform the calculations-----------------------------------------------------
    if (!silent){
        cat(sprintf(paste0("\tCalculating standardised values.",
                           ":\n\t\tWriting to %s.tif\n"
                           ),
                    fileOut
                    )
            )
    }
    for (i in 1:blocks$n){
        if (!silent){
            cat(sprintf("\t\tProcessing block %s of %s\t(%s percent)",
                        i, blocks$n, round(i / blocks$n * 100)
                        )
                )
        }

        #as.matrix to stop errors with a single layer.
        tempValues <- as.matrix(getValues(rasterIn,
                                row = blocks$row[i],
                                nrow = blocks$nrow[i]
                                ))
        if (!silent) {
            cat(".")
        }

        tempValues <- t((t(tempValues) - mv[[1]]) /
                        mv[[3]] * (minMax[2] - minMax[1]) + minMax[1]
                        )
        if (intLock){
            tempValues <- round(tempValues)
        }
        if(!silent){
            cat(".")
        }

        ret <- writeValues(x = ret,
                           v = tempValues,
                           start = blocks$row[i]
                           )
        if (!silent){
            cat(".\n")
        }
    }
  
    ret <- writeStop(ret)

    return(ret)
}
