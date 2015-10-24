PCA <- function(rasterIn, npc = NULL, eigens = NULL, standOut = FALSE,
                fileOut = TempRasterName(), silent = TRUE, retEigen = FALSE){
#Calculates or estimates principal components for an input image.
#
#Currently not able to check if it works as intended. Would need commercial
# software for that. May add the option to sample the data to speed up the
# covariance matrix calculation.
#
#Requires: RasterLoad, RasterShell, Standardise
#
#Args:
#  rasterIn: The raster file to perform principal component analysis on.
#  npc: The number of components to return. Defaults to the number of layers
#   of rasterIn.
#  eigens: Previously calculated eigenvalues to use instead of whole image.
#   Not very transferable between images, but potentially useful to apply
#   eigenvector from a subset of the whole image to the wider area.
#  standOut: Should the end result be 8 bit integer? (using the
#   Standardise function)
#  fileOut: Name of file to save to, defaults to a temporary file.
#  silent: Should the function work without progress reports?
#  retEigen: If eigenvectors should be returned alongside the raster.
#Returns:
#  Depending on retEigen, either a Raster* object or a list with two items:
#   -Raster: The image calculated from principal components.
#   -Eigens: A matrix containing the eigenvectors, eigenvalues & standard devs

    library("raster")
    library("REORS")

    if(!silent) cat("Calculating principal components:\n")
    #Check this works                                                           ToDo
    cat("WARNING: PCA support is currently experimental, use with caution.\n")

    rasterIn <- RasterLoad(rasterIn, "stack")

    if(nlayers(rasterIn) < 2){
        stop("Input raster needs to have more than one layer.\n")
    }
    if(is.null(npc)){
        npc <- nlayers(rasterIn)
    }
    if(npc > nlayers(rasterIn)){
        stop("Cannot calculate more components than input layers.\n")
    }
    blocks <- blockSize(rasterIn)
    #Using a custom method as it's faster, bypasses temp file issues and allows
        #reporting as the function runs.
  
  if(is.null(eigens)){
#--Calculate mean values--------------------------------------------------------
    if(!silent){
        cat("\tCalculating mean values. (Step 1/3)\n")
    }
    mVals <- rep(0, nlayers(rasterIn))
    for(i in 1:blocks$n){
        if(!silent){
            cat(sprintf("\t\tProcessing block %s of %s\t(%s percent)",
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

        mVals <- mVals + colSums(tempValues, na.rm = TRUE)
        if(!silent){
            cat(".\n")
        }
    }

    mVals <- mVals / ncell(rasterIn)
    covMat <- matrix(0, ncol = nlayers(rasterIn), nrow = nlayers(rasterIn))

#--Calculate covariance matrix--------------------------------------------------
    if(!silent){
        cat("\tCalculating covariance matrix. (Step 2/3)\n")
    }
    for(i in 1:blocks$n){
        if(!silent){
            cat(sprintf("\t\tProcessing block %s of %s\t(%s percent)",
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

        tempValues <- t(t(tempValues) - mVals)

        for(j in 1:nlayers(rasterIn)){
            for(k in 1:nlayers(rasterIn)){
                if(j <= k){
                    temp <- sum(tempValues[, j] * tempValues[, k], na.rm = TRUE)
                    covMat[j, k] <- covMat[j, k] + temp
                    covMat[k, j] <- covMat[j, k]
                }
            }
        }
        if(!silent){
            cat(".\n")
        }
    }

    covMat <- covMat / (ncell(rasterIn))

#--Calculate eigenvalues & principal components---------------------------------
    eigens <- eigen(covMat)
    } else {
        if(nrow(eigens) - 3 == npc){
        if(!silent){
            cat("\tReading eigenvectors. (Steps 1 & 2/3)\n")
        }
        eigens <- list(eigens[nlayers(rasterIn) + 1, ],
                       head(eigens, nlayers(rasterIn))
                       )
        } else if(nrow(eigens) != npc){
            stop("Invalid eigenvectors supplied.\n")
        }
    }

    rasterOut <- RasterShell(rasterIn, npc)
    rasterOut <- writeStart(rasterOut,
                            filename = fileOut,
                            format = "GTiff",
                            overwrite = TRUE
                            )

    if(!silent){
        cat("\tCalculating PCA. (Step 3/3)\n")
        cat(sprintf("\tCalculating PCA. (Step 3/3)\n\tWriting to %s.tif\n",
                    fileOut
                    ))
    }
    for(i in 1:blocks$n){
        if(!silent){
            cat(sprintf("\t\tProcessing block %s of %s\t(%s percent)\n",
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

        #tempValues <- tempValues %*% eigens[[2]]
        tempValues <- tempValues %*% eigens[[2]][, 1:npc]
        if(!silent){
            cat(".")
        }

        rasterOut <- writeValues(x = rasterOut,
                                 #v = tempValues[, 1:npc],
                                 v = tempValues,
                                 start = blocks$row[i]
                                 )
        if(!silent){
            cat(".\n")
        }
    }

    rasterOut <- writeStop(rasterOut)

    valMat <- rbind(eigens[[2]],
                    eigens[[1]],
                    eigens[[1]] / sum(eigens[[1]]) * 100,
                    cumsum(eigens[[1]] / sum(eigens[[1]]) * 100)
                    )

    colnames(valMat) <- sprintf("Band %s", 1:ncol(valMat))
    rownames(valMat) <- append(1:ncol(valMat),
                               c("Eigenvectors", "Std Deviation", "cum std dev")
                               )

    if(standOut){
        rasterOut <- Standardise(rasterOut, c(0, 255), TRUE, silent = silent)
    }

    if(retEigen){
        ret <- list("Raster" = rasterOut, "Eigens" = valMat)
    }else{
        ret <- rasterOut
    }
    return(ret)
}
