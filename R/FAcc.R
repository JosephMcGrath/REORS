FAcc <- function(classed, reference, bins = 100, plotOut = FALSE,
 silent = TRUE){
#Visualises the relationship between membership values and reference classes.
#Takes fuzzy membership values from a classification, along with reference
# classes to compare to. Using this, the accuracy is calculated at a range of
# membership values (assuming a pixel is assigned to the class it has the
# highest membership to.
#Where memberships correspond well to a reference class, the accuracy should
# increase with membership.
#
#Args:
#  classed: The fuzzy membership values of the classification to be assessed.
#  reference: The single-layer raster object that represents crisp memberships
#   the classification is to be compared to.
#  plotOut: If the resulting values should be automatically plotted.
#  bins: The number of bins to calculate accuracy for.
#  silent: Should details of the classification be output as it works?
#
#Returns:
#  A matrix of values, with rows for each class and columns for membership
#   values.
  
  library("raster")
  library("REORS")
  
  #Not checking nlayers(classed) to allow for testing a single class.
  if(nlayers(reference) != 1) stop("\"reference\" input must have one layer,")
  
#--Set up bins for fuzzy accuracy measure-------------------------------------
  blocks <- blockSize(classed, n = nlayers(classed) + 1)
  accM <- matrix(0, nlayers(classed), bins + 1)
  colnames(accM) <- seq(0, 1, length.out = bins + 1)
  rownames(accM) <- sprintf("Class %s", 1:nrow(accM))
  
  binCount <- accM
  
  binsV <- seq(0, 1, length.out = bins + 1)
  
  if(!silent) cat("Assessing fuzzy accuracy:\n")
  
#--Run for each block---------------------------------------------------------
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
     
    classedValues <- getValues(
     classed,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    ) 
    referenceValues <- getValues(
     reference,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    #Remove NA cells
    classedValues <- classedValues[!is.na(referenceValues), ]
    referenceValues <- referenceValues[!is.na(referenceValues)]
       
    if(any(classedValues > 1)) stop("Fuzzy memberships cannot be above 1.")
    if(any(classedValues < 0)) stop("Fuzzy memberships cannot be below 0.")
    
    #Round to nearest bin
    classedValues <- round(classedValues * bins) / bins
    
    for(j in 1:nrow(classedValues)){
      tempRow <- which.max(classedValues[j, ])[[1]]
      tempCol <- classedValues[j, tempRow] == binsV
      if(tempRow == referenceValues[j]){
        accM[tempRow, tempCol] <- accM[tempRow, tempCol] + 1
      }
      binCount[tempRow, tempCol] <- binCount[tempRow, tempCol] + 1
    }
  }
  
#--Calculate accuracy percentage----------------------------------------------
  accOut <- accM / binCount
  accOut[binCount == 0] <- NA
  
  if(plotOut){
    temp <- 0
    for(i in 1:ncol(accOut)){
      if(!all(is.na(accOut[, i]))){
        temp <- binsV[i]
      }
    }
    plot(c(0, temp), c(min(accOut, na.rm = TRUE), max(accOut, na.rm = TRUE)),
     xlab = "Membership", ylab = "Accuracy", type = "n")
    for(i in 1:nrow(accOut)){
      lines(binsV, accOut[i, ], col = rainbow(nrow(accOut))[i])
    }
  }
  
  return(accOut)
}
