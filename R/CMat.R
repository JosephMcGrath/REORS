CMat <- function(classed, refernce, retT = "", reOrg = FALSE, stand = TRUE){
#Function to calculate the confusion matrix between two raster layers.
#Calculates a wide variety of accuracy measures.
#Added functionality with unsupervised classifications to link up best
# combinations of classes.
#
#To do notes:
#  Get all measures working properly
#  Add support for cases where validation isn't available for each cell.
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
#  classed: Raster, or name of raster containing hard clustered values.
#  refernce: Raster or name of raster containing reference values
#  retT: One of several methods of data to return:
#   "kappa" will return kappa and accuracy values only.
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

  if(class(classed) == "character") classed <- raster(classed)
  if(class(refernce) == "character") refernce <- raster(refernce)
  
  if(class(classed)[1] != "RasterLayer") stop("Classified layer not loaded")
  if(class(refernce)[1] != "RasterLayer") stop("Reference layer not loaded")
  
  original <- crosstab(classed, refernce)
  
  temp <- matrix(ncol = sqrt(nrow(original)), nrow = sqrt(nrow(original)))
  for(i in 1:sqrt(nrow(original))){
    temp[seq(1,sqrt(nrow(original))), i] <- original[
     seq(i * sqrt(nrow(original)) - 4, i * sqrt(nrow(original))), 3]
  }
  temp <- temp[seq(1, nrow(temp) - 1), seq(1, ncol(temp) - 1)]
  
#--Re-organise classes to best fit--------------------------------------------
  if(reOrg){
#Shuffle around classes, so that largest classified classes match with the 
 #largest "confirmed" classes.

#Outputs which true class is highest for each classified
    large <- c()
    for (i in 1:dim(temp)[1]){
      large <- append(large,
       max((max(temp[i,])==temp[i,])* 1:dim(temp)[2]))
    #Use of max is to deal with occasions where several are tied
    #If this is changed needs to be mirrored below
    }
  
#Make a copy of the confusion matrix to work with
   sMat <- temp
  
  #While all aren't unique
   #Set the lowest "max" value to -1 to remove it from consideration.
   #(-1 as that is lower than values may start out)
    for (donotuse in 1:length(sMat)){ #not a while to give it a definite end
     if (length(unique(large)) == length(large)) break
    #There's probably a better way to check for uniqueness
    
#Set an appropriate value to 0
      for (i in 1:length(large)){
        if (sum(large==i) > 1){
          sMat[sMat == min(
           sMat[(1:dim(temp)[1])[large==i],i])]  <- 0
        }
      }
    
#Then recalculate the list of largest class
      large <- c()
      for (i in 1:dim(sMat)[1]){
        large <- append(large,
         max((max(sMat[i,])== sMat[i,])* 1:dim(sMat)[2]))
      }
      if (length(unique(large)) == length(large)) break
    }
  
#Move the relevant items
    if (max(summary(as.factor(large))) == 1){
      for (i in 1:dim(temp)[1]){
        sMat[i,] <- temp[(1:length(large))*(large==i),]
      }
    } else {
      warning("Error - this may be due to empty classes")
      sMat <- temp
    }
  } else {
    large <- seq(1, ncol(temp))
    sMat <- temp
  }
  
#--Standardise the table between 0 and 1--------------------------------------
  if(stand){
    sMat <- (sMat - min(sMat)) / (max(sMat) - min(sMat))
  }
	
#--Calculate accuracy measures------------------------------------------------
  
#Overall Accuracy
  tObs <- sum(sMat)
  oAcc <- 0
  for (i in 1:dim(sMat)[1]){
    oAcc <- oAcc + sMat[i,i]
  }
  oAcc <- oAcc / tObs
  
#Producers accuracy
  pAcc <- c()
  for (i in 1:dim(sMat)[1]){
    pAcc[i] <- sMat[i,i] / sum(sMat[,i])
  }
  
#Users accuracy
  uAcc <- c()
  for (i in 1:dim(sMat)[1]){
    uAcc[i] <- sMat[i,i] / sum(sMat[i,])
  }
  
#Kappa
  tPos <- 0
  mTot <- 0
  
  for (i in 1:dim(sMat)[1]){
    tPos <- tPos + sMat[i,i]
  }
  for (i in 1:dim(sMat)[1]){
    mTot <- mTot + sum(sMat[i,]) * sum(sMat[,i])
  }
  
  kppa <- (tObs * (tPos) - (mTot)) / ( tObs ^ 2 - (mTot))

#Quantity disagreement
  #Per class
  qDiss <- c()
  for(i in 1:ncol(sMat)){
    qDiss <- append(qDiss, sum(sMat[, i]) - sum(sMat[i, ]))
  }
  #qg = sum(sum(sMat
  #Overall
  qDissSum <- sum(qDiss) / 2

#Allocation disagreement
  #Per class
  aDiss <- c()
  for(i in 1:ncol(sMat)){
    aDiss <- append(aDiss, 2 * min(sum(sMat[, i]) - sMat[i, i],
     sum(sMat[i, ]) - sMat[i, i]))
  }
  
  #Overall
  aDissSum <- sum(aDiss) / 2
  

#Overall disagreement
  tDiss <- qDissSum + aDissSum

  #Check proportion correct = accuracy
  
#--Return requested statistics------------------------------------------------

  if (retT == "kappa"){
    ret <- c(kppa,oAcc)
  } else if (retT == "order"){ 
    ret <- large
  }else {
    ret <- list(
     adjusted = sMat,
     kappaValue = as.vector(kppa),
     producersAccuracy = pAcc,
     usersAccuracy = uAcc,
     overallAccuracy = as.vector(oAcc),
#Need to add overall u&p accuracies and fix disagreement measures
     #quantityDisagreement = qDiss,
     #allocationDisagreement = aDiss,
     #totalQuantityDisagreement = qDissSum,
     #totalAllocationDisagreement = aDissSum,
     #totalDisagreement = tDiss,
     reorganising = large,
     unadjusted = temp
    )
  }
  
  return(ret)
}
