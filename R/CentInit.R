CentInit <- function(rasterIn, nCentres, meth = "rand", silent = TRUE){
#Several methods of generating initial centres for a centroid-based clustering
# algorithm. A few are also implemented as part of the algorithms, but the
# more time-intensive ones are isolated here.
#
#Requires: RasterLoad
#
#Args:
#  rasterIn: The Raster* object to use, passed through RasterLoad.
#  nCentres: How many clusters should the data be split into?
#  meth: The method used to initialise centres, one of:
#   -"rand" : A simple random distribution between the minimum and maximum
#    values per layer.
#   -"lin"  : Distributed linearly between the minimum and maximum values
#    per layer.
#   -"prin" : (NYI) Linear distribution along the first principal component.
#   -"dist" : Distributed randomly, but with probability based on data in.
#  silent: Should the function work without progress reports?
#
#Returns:
#  A matrix of cluster centres with columns as centres and rows as layers.

    library("raster")
    library("REORS")

    #Check & clean up input
    if (!is.character(meth)){
        stop("\"meth\" must be a character vector.\n")
    }
    meth <- tolower(meth)
    if (meth == "linear"){
        meth <- "lin"
    }
    if (meth == "principal"){
        meth <- "prin"
    }
    if (meth == "principal components"){
        meth <- "prin"
    }
    if (meth == "random"){
        meth <- "rand"
    }
    if (meth == "distributed"){
        meth <- "dist"
    }
    if (meth == "distributed random"){
        meth <- "dist"
    }

    rasterIn <- RasterLoad(rasterIn, "stack")
    #rasterIn <- setMaxMin(rasterIn)

    ret <- matrix(ncol = nCentres, nrow = nlayers(rasterIn))
    colnames(ret) <- sprintf("Clust %s", 1:nCentres)
    rownames(ret) <- names(rasterIn)

#--Random generation------------------------------------------------------------
    if (meth == "rand"){
        for (i in 1:nrow(ret)){
            ret[i, ] <- runif(n   = nCentres,
                              min = minValue(rasterIn)[i],
                              max = maxValue(rasterIn)[i]
                              )
        }
    }

#--Linear generation------------------------------------------------------------
    if (meth == "lin"){
        for (i in 1:nrow(ret)){
            ret[i, ] <- seq(from = minValue(rasterIn)[i],
                            to = maxValue(rasterIn)[i],
                            length.out = nCentres
                            )
        }
    }

#--Linear principal components--------------------------------------------------
    #Distributing initial centroids along the principal component may give
        #better representation of data rather than between min/max values.
    if (meth == "prin"){
        #Need to work out how to bring the data back out of principal componentsToDo
        stop("Not yet implemented.\n")

        #Uses the same method as PCA
        blocks <- blockSize(rasterIn)
        #Calculate mean values
        if (!silent){
            cat("Calculating mean values. (Step 1/3)\n")
        }
        mVals <- rep(0, nlayers(rasterIn))
        for (i in 1:blocks$n){
            if (!silent){
                cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
                            i,
                            blocks$n, round(i / blocks$n * 100)
                            ))
            }

            tempValues <- getValues(x    = rasterIn,
                                    row  = blocks$row[i],
                                    nrow = blocks$nrow[i]
                                    )

            mVals <- mVals + colSums(tempValues, na.rm = TRUE)
        }

        mVals <- mVals / ncell(rasterIn)
        covMat <- matrix(0, ncol = nlayers(rasterIn), nrow = nlayers(rasterIn))

        #Calculate covariance matrix
        if (!silent){
            cat("Calculating covariance matrix. (Step 2/3)\n")
        }
        for (i in 1:blocks$n){
            if (!silent){
                cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
                            i,
                            blocks$n,
                            round(i / blocks$n * 100)
                            ))
            }

            tempValues <- getValues(x    = rasterIn,
                                    row  = blocks$row[i],
                                    nrow = blocks$nrow[i]
                                    )

            tempValues <- t(t(tempValues) - mVals)

            for (j in 1:nlayers(rasterIn)){
                for (k in 1:nlayers(rasterIn)){
                    if (j <= k){
                        temp <- sum(tempValues[, j] * tempValues[, k],
                                    na.rm = TRUE
                                    )
                        covMat[j, k] <- covMat[j, k] + temp
                        covMat[k, j] <- covMat[j, k]
                    }
                }
            }
        }

        covMat <- covMat / (ncell(rasterIn) - 1)

        #Calculate eigenvalues
        eigens <- eigen(covMat)

        #Might be necessary to calculate first component?                       ToDo
        #Then calculate the linear path from min to max
        #And transfer back to actual feature-space.
    }

#--Random, weighted by data distribution----------------------------------------
    if (meth == "dist"){
        if (!silent){
            cat("Calculating density of input.\n")
        }
        den <- density(rasterIn, plot = FALSE)
        if (!silent){
            cat("Calculating centres matrix.\n")
        }
        for (i in 1:nrow(ret)){
            ret[i, ] <- sample(x = den[[i]]$x,
                               n = nCentres,
                               prob = den[[i]]$y,
                               replace = TRUE
                               ) + runif(1, 0, den[[i]]$bw)
        }
    }

#--Print and return output------------------------------------------------------
    if (!silent){
        cat("Initial centroids:\n")
        print(ret)
    }

    return(ret)
}
