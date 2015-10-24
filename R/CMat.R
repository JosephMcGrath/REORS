CMat <- function(classed, reference, retT = "Full", reOrg = FALSE,
                 stand = TRUE){
#Calculates the confusion matrix between two classified raster layers.
#Calculates a wide variety of accuracy measures.
#Added functionality with unsupervised classifications to link up best
# combinations of classes.
#
#To do notes:
#  Get all measures working properly
#  Add support for cases where validation isn't available for each cell.
#  Option to take an equal sample from each class to avoid biases when
#   reorganising.
#
#Source:
#  Congalton R.G. 1991 
#   A review of assessing the accuracy of classifications of remotely sensed
#   data. Remote sensing of environment. Volume 37, p35-46.
#  Pontius R.G. Millones M. 2011
#   Death to Kappa: birth of quantity disagreement and allocation disagreement
#   for accuracy assessment. International Journal of Remote Sensing.
#   Volume 32, p4407-4429.
#
#Args:
#  classed: RasterLayer, or name of raster containing hard clustered values.
#  reference: RasterLayer or name of raster containing reference values.
#  retT: One of several methods of data to return:
#   "brief" will return kappa and accuracy values only.
#   "order" will return the sequence the classes were reorganised into.
#   All other values will return the full set of data.
#  reOrg: If the values should be reorganised based on how well the classes
#   fit the reference values. Mainly useful for unsupervised classifications,
#   where classes are assigned in a random/meaningless order.
#  stand: If the confusion matrix should be standardised (locked between 0 and
#   1). Makes it more comparable between classifications with different
#   numbers of pixels.
#
#Returns:
#  Either a list with all the calculated variables and associated statistics,
#   or a cut down version of the list:
#  Key:
#   adjusted: confusion matrix after any reorganisation or standardisation
#   kappaValue: the kappa value for the confusion matrix
#   producersAccuracy: the produces accuracy for the matrix
#   usersAccuracy: the users accuracy for the confusion matrix
#   overallAccuracy: the overall accuracy value for the confusion matrix
#   quantityDisagreement: quantity disagreement for each class (NYI)
#   allocationDisagreement: allocation disagreement for each class (NYI)
#   totalQuantityDisagreement: quantity disagreement for all classes (NYI)
#   totalAllocationDisagreement:quantity disagreement for all classes (NYI)
#   totalDisagreement: total disagreement between layers (NYI)
#   reorganising: the order used to reorganise the clusters
#   unadjusted: the confusion matrix as it was originally calculated

    library("raster")
    
    # Input's a bit more rigid than other functions, only single RasterLayers
    if (class(classed) == "character"){
        classed <- raster(classed)
    }
    if (class(reference) == "character"){
        reference <- raster(reference)
    }

    if (class(classed)[1] != "RasterLayer"){
        stop("Classified layer must be a RasterLayer.\n")
    }
    if (class(reference)[1] != "RasterLayer"){
        stop("Reference layer must be a RasterLayer.\n")
    }
    
    #Uses the raster library's crosstab function to do the heavy lifting.
    #May need to replace with multicore version if I go that route.             #Todo
    original <- crosstab(classed, reference)
    
    #Reorganise to a format that's easier to run with.
    temp <- matrix(ncol = sqrt(nrow(original)), nrow = sqrt(nrow(original)))
    for (i in 1:sqrt(nrow(original))){
        temp[, i] <- original[seq((i - 1) * nrow(temp) + 1, i * nrow(temp)), 3]
    }
    #Drop the rows and columns relating to NA values.
    temp <- temp[seq(1, nrow(temp) - 1), seq(1, ncol(temp) - 1)]

#--Re-organise classes to best fit--------------------------------------------
    if(reOrg){
        #Shuffle around classes, so that largest classified classes match with
            #the largest "confirmed" classes.

        #Outputs which true class is highest for each classified
        large <- rep(NA, nrow(temp))
        for (i in 1:nrow(temp)){
            #Simplest to only take one item when tied - taking the first one.
            #This is mirrored below, changes need to be copied down.
            large[i] <- which(max(temp[i, ]) == temp[i, ])[1]
        }

        #temp is kept as a record of the original. Using proportions of 
            #reference classes, to reduce effects from different sizes
            #for each class.
        sMat <- temp / rowSums(temp)

        #Remove smallest items from temp matrix until best pairs remain
        for (doNotUse in 1:length(sMat)){
            if (length(unique(large)) == length(large)){
                break
            }

            for (i in unique(large)){
                if (sum(large == i) > 1){
                    use <- which(large == i) + (i - 1) * nrow(sMat)

                    #min() is somewhat arbitrary here
                    sMat[min(use[sMat[use] == min(sMat[use])])] <- NA
                }
            }

            large <- rep(NA, nrow(sMat))
            for (i in 1:nrow(sMat)){
                large[i] <- which(max(sMat[i, ], na.rm = TRUE) == sMat[i, ])[1]
            }
        }

        #Move the relevant items
        if (length(unique(large)) == length(large)){
            for (i in 1:nrow(temp)){
            #    sMat[i,] <- temp[(1:length(large)) * (large == i), ]
            sMat[i,] <- temp[(large == i), ]
            }
        } else {
            stop("Error - this may be due to empty classes")
        }
    } else {
        large <- seq(1, ncol(temp))
        sMat <- temp
    }

#--Standardise the table between 0 and 1--------------------------------------
    if(stand){
        sMat <- sMat / max(sMat)
    }
    
#--Calculate accuracy measures------------------------------------------------
    tObs <- sum(sMat)

    #Overall Accuracy
    oAcc <- sum(diag(sMat)) / tObs

    #Producers accuracy
    pAcc <- rep(NA, nrow(sMat))
    for (i in 1:nrow(sMat)){
        pAcc[i] <- sMat[i,i] / sum(sMat[,i])
    }
  
    #Users accuracy
    uAcc <- rep(NA, nrow(sMat))
    for (i in 1:nrow(sMat)){
        uAcc[i] <- sMat[i,i] / sum(sMat[i,])
    }

    #Kappa
    tPos <- 0
    mTot <- 0

    for (i in 1:nrow(sMat)){
        tPos <- tPos + sMat[i,i]
        mTot <- mTot + sum(sMat[i,]) * sum(sMat[,i])
    }
    kppa <- (tObs * tPos - mTot) / (tObs ^ 2 - mTot)
    
    #Set up for disagreement measures
        #refered to as "unbiased population matrix" in Pontius & Millones
        #May be worth investigating further. Maybe investigate for more use.
    pMat <- sMat 

    for(i in 1:nrow(pMat)){
        for(j in 1:ncol(pMat)){
          pMat[i, j] <- sMat[i, j] / sum(sMat[i, ]) *
                        sum(sMat[j, ]) / sum(sMat)
        }
    }
    print(pMat)

#Quantity disagreement
    #Per class
    qDiss <- rep(NA, nrow(sMat))
    for(i in 1:nrow(sMat)){
        qDiss[i] <- abs(sum(pMat[, i]) - sum(pMat[i, ]))
    }
    #Overall
    qDissSum <- sum(qDiss) / 2

    #Allocation disagreement
    #Per class
    aDiss <- rep(NA, nrow(sMat))
    for(i in 1:nrow(sMat)){
        aDiss[i] <- 2 * min(sum(pMat[, i]) - pMat[i, i],
                            sum(pMat[i, ]) - pMat[i, i]
                            )
    }
    #Overall
    aDissSum <- sum(aDiss) / 2

    #Overall disagreement
    tDiss <- qDissSum + aDissSum

    #Check proportion correct = accuracy
    #print(tDiss - (1 - sum(diag(pMat))))

#--Return requested statistics------------------------------------------------

    if (retT == "brief"){
        ret <- c(kppa,oAcc)
    } else if (retT == "order"){ 
        ret <- large
    } else {
        ret <- list(adjusted = sMat,
                    kappaValue = as.vector(kppa),
                    producersAccuracy = pAcc,
                    usersAccuracy = uAcc,
                    overallAccuracy = as.vector(oAcc),
                    quantityDisagreement = qDiss,
                    allocationDisagreement = aDiss,
                    totalQuantityDisagreement = qDissSum,
                    totalAllocationDisagreement = aDissSum,
                    totalDisagreement = tDiss,
                    reorganising = large,
                    #UnbiasedPopulation = pMat,
                    unadjusted = temp
                    )   
    }

    return(ret)
}
