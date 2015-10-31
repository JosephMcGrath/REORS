Geomorphometry <- function(rasterIn, funIn, kernelSize, vEx = 1,
                           kernelShape = "circle", na.rm = FALSE,
                           fileOut = TempRasterName(), silent = TRUE){
#A wrapper function for FocalCalc containing a number of pre-defined functions
# to calculate geomorphometric derivatives of a DEM.
#
#Requires:
#  FocalCalc
#
#Hypsometric integral source: 
# Pike,R.J., Wilson,S.E. (1971). 
# Elevation-relief ratio, hypsometric integral, and geomorphic area-altitude
# analysis. Geological Society of America Bulletin, 82, 1079-1084
#
#Topographic openness source:
# Yokoyama R., Shirasawa M., Pike R.J. (2002)
# Visualizing Topography by Openness: A New Application of Image Processing to
# Digital Elevation Models. Photogrammetric Engineering & Remote Sensing.
# 68, 257-265
#
#Args:
#  rasterIn: The raster to be processed, probably a DEM.
#  function: A string naming the function to be used, options are:
#   -"hyps": The hypsometric integral
#   -"range": The difference between the maximum and minimum values in the
#    kernel.
#   -"amin": The difference between the centre of the kernel and the minimum
#    elevation in the kernel.
#   -"bmax": The difference between the centre of the kernel and the maximum
#    elevation in the kernel.
#   -"topoOpennessP": The positive topographic openness. Assumes the input is
#    a DEM and that horizontal and vertical units are the same.
#   -"topoOpennessN": The negative topographic openness. Assumes the input is
#    a DEM and that horizontal and vertical units are the same.
#  vEx: The exaggeration between the resolution of the raster and the
#   vertical units of the DEM. e.g. if they are both in metres, then this
#   would be 1.
#
#Returns:
#  A raster with the same number of layers as the input.

    if(class(funIn) != "character"){
        stop("Non-character given for funIn.")
#--Hypsometric integral---------------------------------------------------------
    } else if(funIn == "hyps"){
        sumFun <- function(x, na.rm){
            if(na.rm){
                x = x[!is.na(x)]
            }
            return((mean(x) - min(x)) / (max(x) - min(x)))
        }
#--Range of values--------------------------------------------------------------
    } else if(funIn == "range"){
        sumFun <- function(x){
            if(na.rm){
                x = x[!is.na(x)]
            }
            return(max(x) - min(x))
        }
#--Amount the centre pixel is greater than the minimum value--------------------
    } else if(funIn == "amin"){
        sumFun <- function(x){
            if(na.rm){
                x = x[!is.na(x)]
            }
            return(x[ceiling(length(x) / 2)] - min(x))
        }
#--Amount the centre pixel is less than the maximum value-----------------------
    } else if(funIn == "bmax"){
        sumFun <- function(x){
            if(na.rm){
                x = x[!is.na(x)]
            }
            return(max(x) - x[ceiling(length(x) / 2)])
        }
#--Positive and negative topographic openness-----------------------------------
    } else if(funIn %in% c("topoOpennessP", "topoOpennessN")){
        #This section is copied directly from FocalCalc
        noMod <- FALSE
        if(class(kernelSize) == "matrix"){
            kernelUse <- kernelSize
            noMod <- TRUE
        } else if(class(kernelSize) == "numeric" |
                  class(kernelSize) == "integer"){
            if(length(kernelSize) == 2){
                kernelUse <- matrix(1,
                                    ncol = kernelSize[1],
                                    nrow = kernelSize[2]
                                    )
            } else if(length(kernelSize) == 1){
                kernelUse <- matrix(1, ncol = kernelSize, nrow = kernelSize)
            } else {
                #Should not have indentical error messages                      ToDo
                stop("Invalid kernel definition.")
            }
        } else {
            stop("Invalid kernel definition.")
        }

        #Apply shape to the weightings
        if(!noMod){
            if(kernelShape == "circle"){
                mid <- c(ceiling(ncol(kernelUse) / 2),
                         ceiling(nrow(kernelUse) / 2)
                         )
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
                    kernelUse[i, j] <- 1 / (2 * pi * (1 ^ 2)) *
                                       exp(-1 * (((i - mid[1]) /
                                       (0.2 * ncol(kernelUse))) ^ 2 +
                                       ((j - mid[2]) /
                                       (0.2 * nrow(kernelUse))) ^ 2) / 2 *
                                       1 ^ 2 #The 1 in 1 ^ 2 here is sigma
                                       )
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
        #End of section copied from FocalCalc

        kernelDist <- matrix(NA, ngbSize[1], ngbSize[2])
        cent <- ceiling(ngbSize / 2)
        for(i in 1:nrow(kernelDist)){
            for(j in 1:nrow(kernelDist)){
                kernelDist[i, j] <- sqrt(((i - cent[1]) *
                                         res(rasterIn)[1]) ^ 2 +
                                         ((j - cent[2]) * res(rasterIn)[2]) ^ 2
                                         )
            }
        }
        kernelDist[cent[1], cent[2]] <- NA

        mid <- ceiling(length(kernelDist) / 2)
        diam <- floor(nrow(kernelDist) / 2)
        pulls <- list(seq(1, mid, nrow(kernelDist) + 1),
                      seq(mid, nrow(kernelDist), -1 * nrow(kernelDist) + 1),
                      seq(mid, length(kernelDist), nrow(kernelDist) + 1),
                      seq(mid, length(kernelDist) - nrow(kernelDist) + 1,
                      nrow(kernelDist) - 1),
                      seq(ceiling(nrow(kernelDist) / 2), mid, nrow(kernelDist)),
                      seq(mid, length(kernelDist) - diam, nrow(kernelDist)),
                      seq(mid - diam, mid),
                      seq(mid, mid + diam)
                      )

        kernelDist <- kernelDist[!is.na(kernelUse)]
        for(i in 1:length(pulls)){
            pulls[[i]] <- pulls[[i]][!(pulls[[i]] %in% which(is.na(kernelUse)))]
        }

        #Define the actual functions here
        if(funIn == "topoOpennessP"){
            sumFun <- function(x){
                get("pulls")
                x <- atan((x - x[mid]) / kernelDist) / (2 * pi) * 360
                return(mean(
                            max(x[pulls[[1]]], na.rm = TRUE),
                            max(x[pulls[[2]]], na.rm = TRUE),
                            max(x[pulls[[3]]], na.rm = TRUE),
                            max(x[pulls[[4]]], na.rm = TRUE),
                            max(x[pulls[[5]]], na.rm = TRUE),
                            max(x[pulls[[6]]], na.rm = TRUE),
                            max(x[pulls[[7]]], na.rm = TRUE),
                            max(x[pulls[[8]]], na.rm = TRUE)
                            )
                       )
            }
        } else if(funIn == "topoOpennessN"){
            sumFun <- function(x){
                get("pulls")
                x <- atan((x - x[mid]) / kernelDist) / (2 * pi) * 360
                return(mean(min(x[pulls[[1]]], na.rm = TRUE),
                            min(x[pulls[[2]]], na.rm = TRUE),
                            min(x[pulls[[3]]], na.rm = TRUE),
                            min(x[pulls[[4]]], na.rm = TRUE),
                            min(x[pulls[[5]]], na.rm = TRUE),
                            min(x[pulls[[6]]], na.rm = TRUE),
                            min(x[pulls[[7]]], na.rm = TRUE),
                            min(x[pulls[[8]]], na.rm = TRUE)
                            )
                       )
            }
        }
    } else {
        stop(sprintf("\"%s\" is not a valid input function.", funIn))
    }

    ret <- FocalCalc(rasterIn,
                     funIn,
                     kernelSize,
                     kernelShape = "circle",
                     na.rm = FALSE,
                     fileOut = TempRasterName(),
                     silent = TRUE
                     )

    return(ret)
}
