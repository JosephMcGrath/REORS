FocalCalc <- function(rasterIn, sumFun, kernelSize, kernelShape = "circle",
                      na.rm = FALSE, fileOut = TempRasterName(), silent = TRUE){
#Takes the area surrounding a given Raster* object (one or several layers) and
# summarises them based on a used-defined, or pre-set function.
#
#This function adds support for multi-layered Raster* objects, which at the
# time of writing are not supported by the raster package's "focal" function.
#
#Requires: RasterLoad, RasterShell
#
#Args:
#  rasterIn: The raster file to use in calculation, passed through RasterLoad
#  sumFum: The function used to summarise the focal values. Any function that
#   takes in a range of values will generally work (must accept an na.rm
#   argument, even if it does not actually use it). If multiple values are
#   returned, it will only use the first.
#  kernelSize: The size of the kernel to be used. May be either a single
#   number (for a square matrix), a pair of numbers (for uneven height &
#   width). Alternatively a pre-set matrix can be given (in which case
#   kernelShape will be ignored), NA values can be given for pixels to be
#   ignored. Note: kernel MUST have odd dimensions, can use NA values to
#   simulate even sides.
#  kernelShape: The weighting/shape to give the kernel. The values in the
#   kernel are multiplied by these - so take this into account when using
#   non NA/1 values. The following options are available:
#   -"square": All values in the kernel are set to 1
#   -"circle": A circle of 1 values in the centre of the matrix, surrounded
#    by NA values. With smaller kernels, the shape can be distorted.
#   -"gaussian: Decreasing weighting outwards, based on Gaussian distribution.
#    Sigma value for distribution is set to 1.
#  na.rm: Ignores NA values in the Raster* input.
#  fileOut: Name to be used in output, defaults to temporary file.
#  silent: Should details of the classification be output as it works?
#
#Returns:
#  A Raster* object with the same number of layers as input containing the
#   calculated values.

    library("raster")
    library("REORS")

    rasterIn <- RasterLoad(rasterIn, retForm = "stack")

    if(kernelSize %% 2 != 1){
        stop("Kernel must have odd number of cells per side.\n")
    }

#--Set up the weighting matrix-------------------------------------------------
  #NOTE: This section is also used in the Geomorphometry function. If this is
   #updated, copy all changes across.
  
    #Generate the initial values
    noMod <- FALSE
    if(class(kernelSize) == "matrix"){
        kernelUse <- kernelSize
        noMod <- TRUE
    } else if(class(kernelSize) == "numeric" |
              class(kernelSize) == "integer"){
        if(length(kernelSize) == 2){
            kernelUse <- matrix(1, ncol = kernelSize[1], nrow = kernelSize[2])
        } else if(length(kernelSize) == 1){
            kernelUse <- matrix(1, ncol = kernelSize, nrow = kernelSize)
        } else {
            stop("Invalid kernel definition.")
        }
    } else {
        stop("Invalid kernel definition.")
    }

    #Apply shape to the weightings
    if(!noMod){
        if(kernelShape == "circle"){
            mid <- c(ceiling(ncol(kernelUse) / 2), ceiling(nrow(kernelUse) / 2))
            for(i in 1:ncol(kernelUse)){
                for(j in 1:nrow(kernelUse)){
                    if(sqrt((i - mid[1]) ^ 2 +
                       (j - mid[2]) ^ 2) > (mean(mid) - 1)){
                        kernelUse[i, j] <- NA
                    }
                }
            }
        } else if(kernelShape == "gaussian"){
            mid <- c(ceiling(ncol(kernelUse) / 2), ceiling(nrow(kernelUse) / 2))
            for(i in 1:ncol(kernelUse)){
                for(j in 1:nrow(kernelUse)){
                    #This could really do with cleaning up                      ToDo
                    kernelUse[i, j] <- 1 / (2 * pi * (1 ^ 2)) *
                                       exp(-1 * (((i - mid[1]) /
                                           (0.2 * ncol(kernelUse))) ^ 2 +
                                           ((j - mid[2]) /
                                           (0.2 * nrow(kernelUse))) ^ 2) / 2 *
                                           1 ^ 2
                                           ) #The 1 in 1 ^ 2 here is sigma value
                }
            }
        }
    }

    if(!silent){
        cat("Kernel used:\n")
        print(kernelUse)
    }

    #Finally "flatten" out the kernel
    ngbSize <- c(nrow(kernelUse), ncol(kernelUse))
    kernelUse <- c(kernelUse)

#--Apply the filter to the data-----------------------------------------------
  #Using conservatively small blocks to keep things manageable.
  #Results in more read/write cycles.
  #n = kernel * nlayers? For Raster*'s with many layers.
    blocks <- blockSize(rasterIn, n = length(kernelUse))
    rasterOut <- RasterShell(rasterIn)

    rasterOut <- writeStart(rasterOut, filename = fileOut, format = "GTiff",
    overwrite = TRUE)

    if(!silent){
        cat(sprintf("Applying focal operation:\nWriting to %s.tif\n",
                    fileOut
                    ))
    }

    for(i in 1:blocks$n){
        if(!silent){
            cat(sprintf("\tProcessing block %s of %s\t(%s percent)",
                        i, blocks$n, round(i / blocks$n * 100)
                        ))
        }

        tempValues <- getValuesFocal(rasterIn,
                                     row = blocks$row[i],
                                     nrow = blocks$nrow[i],
                                     ngb = ngbSize,
                                     padValue = NA
                                     )
        if(!silent){
            cat(".")
        }

        #These want vectorising, massively inefficient here.
        if(nlayers(rasterIn) == 1){
            writeV <- rep(NA, nrow(tempValues))
            tempValues <- t(t(tempValues) * kernelUse)
            for(j in 1:nrow(tempValues)){ #Using "j" for easy conversion below
                if(!any(is.na(tempValues[j, !is.na(kernelUse)])) | na.rm){
                    writeV[j] <- sumFun(tempValues[j, ], na.rm = na.rm)[[1]]
                }
            }
        } else {
            writeV <- matrix(NA,
                             nrow = nrow(tempValues[[1]]),
                             ncol = length(tempValues)
                             )
            for(k in 1:length(tempValues)){
                tempValues[[k]] <- t(t(tempValues[[k]]) * kernelUse)
                for(j in 1:nrow(tempValues[[k]])){
                    #Clean this section above                                   ToDo        
                    if(!any(is.na(tempValues[[k]][j, !is.na(kernelUse)])) |
                       na.rm){
                        writeV[j, k] <- sumFun(tempValues[[k]][j, ][
                                            !is.na(tempValues[[k]][j, ])
                                            ])[[1]]
                    }
                }
            }
        }
        if(!silent){
            cat(".")
        }

        rasterOut <- writeValues(x = rasterOut,
                                 v = writeV,
                                 start = blocks$row[i]
                                 )
        if(!silent){
            cat(".\n")
        }
    }
  
#--Finish writing and return the final values---------------------------------  
    rasterOut <- writeStop(rasterOut)

    return(rasterOut)
}
