KMeans <- function(rasterIn, nCentres = 10, its = 1, weight = 1,
 fileOut = tempfile(pattern = "REORS"), breakCon = 0.01, standIn = FALSE,
 distM = "euc", silent = TRUE, interPlot = FALSE){
#Standard k-means clustering algorithm.
#Somewhat of a work in progress, data still needs to be standardised before
# input to avoid artificial weighting and weighting of inputs still
# needs to be added.
#
#Requires: RasterLoad, RasterShell, Standardise
#
#ToDo:
#Could probably be sped up by replacing apply with matrix algebra. Pretty big
# overhaul though.
#
#Args:
#  rasterIn: Name of the image file to classify. Maybe run it through a
#   cleaning function first?
#  nCentres: How many clusters should the data be split into? Empty clusters
#   are deleted as the algorithm progresses - may change this later.
#  its: Maximum number of iterations to run the algorithm for.
#  weight: The weights to apply to each layer (higher weight means an greater
#   importance to clustering, can be integers or decimals. Must be a single
#   values or have length equal to the number of input layers.
#  fileOut: Name to write file to, defaults to temporary file.
#  breakCon: How little variation between iterations will break the loop early
#   calculated as an average per variable, assuming variables are between 0-1.
#  standIn: Should the data be locked between 0 and 1 before classification?
#  distM: Distance measure to calculate memberships:
#    - "euc": euclidean distance
#    - "man": Manhattan distance
#    - "eu2": squared euclidean distance
#  silent: Should details of the classification be output as it works?
#  interPlot: Should the classification be plotted each iteration?
#
#Returns:
#  Image (also written to disk) with classes represented by numerical values.

#--Set up and build the initial centres---------------------------------------
  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(length(weight) != 1 & length(weight) != nlayers(rasterIn)){
    stop("Weights must have values for each layer of input or a single value")
  }
  
  if(silent){
    if(standIn) rasterIn <- Standardise(rasterIn, c(0, 1))
  } else {
    if(standIn) rasterIn <- Standardise(rasterIn, c(0, 1), silent = FALSE)
  }
  
  if(is.na(max(maxValue(rasterIn)))) rasterIn <- setMinMax(rasterIn)
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- RasterShell(rasterIn, 1)
  
  centres <- matrix(ncol = nCentres, nrow = nlayers(rasterIn))
  colnames(centres) <- sprintf("c%s", 1:nCentres)
  rownames(centres) <- sprintf("Layer %s", 1:nlayers(rasterIn))
  
  for(i in 1:nlayers(rasterIn)){
    centres[i, ] <- seq(
     from = minValue(rasterIn)[i],
     to = maxValue(rasterIn)[i],
     length.out = nCentres
    )
  }
  
  if(!silent){
    cat("Initial centres:\n")
    print(centres)
  }
  
#--Set up distance measure of choice------------------------------------------
  if(distM == "euc"){
    distM <-  function(x, y) return(sqrt(colSums((x - y) ^ 2)))
  } else if(distM == "man"){
    distM <- function(x, y) return(colSums(abs(x - y)))
  } else if("eu2"){
    distM <- function(x, y) return(colSums((x - y) ^ 2))
  } else stop("Invalid distance measure")
  
#--Run the algorithm for each iteration---------------------------------------
  for(i in 1:its){
    
    if(!silent) cat(sprintf("Beginning iteration %s of %s\n", i, its))
    
    tempCentres <- centres * 0
    classCount <- rep(0, ncol(centres))
    
    rasterTemp <- writeStart(rasterTemp, filename = fileOut,
     format = "GTiff", overwrite = TRUE
    )
    
#--Calculate memberships and new centres--------------------------------------
    for(j in 1:blocks$n){
      if(!silent) cat(sprintf("\tProcessing block %s of %s\n", j, blocks$n))
      
      tempValue <- getValues(
       rasterIn,
       row = blocks$row[j],
       nrow = blocks$nrow[j]
      )
      
      tempClass <- rep(NA, nrow(tempValue))
      
      for(k in 1:nrow(tempValue)){
        temp <- distM(tempValue[k, ] * weight, centres)
        tempClass[k] <- sum(1:length(temp) * (temp == min(temp)))
      }
      
      rasterTemp <- writeValues(
       x = rasterTemp,
       v = tempClass,
       start = blocks$row[j]
      )
      
      for(k in 1:ncol(centres)){
        classCount[k] <- classCount[k] + sum(tempClass == k, na.rm = TRUE)
        tempCentres[, k] <- tempCentres[, k] +
         colSums(matrix(tempValue[k == tempClass, ], ncol = nrow(centres)),
          na.rm = TRUE
         )
      }
      
    }
    
    rasterTemp <- writeStop(rasterTemp)
    newCentres <- t(t(tempCentres) / classCount)
    
    #is.nan() used to compensate for clusters with 0 pixels in the cluster
    diffSince <- sum(abs(centres[, !is.nan(colSums(newCentres))] - 
      newCentres[, !is.nan(colSums(newCentres))])) / ncol(centres)
    if(!silent) cat(sprintf("%s difference since last iteration.\n",
     round(diffSince, 3)))
    
    if(diffSince < breakCon) {
      if(!silent) cat("Converged, breaking loop.\n")
      break
    }
    
    #Keeping centres with no changes as they are.
    #centres <- newCentres[, !is.nan(colSums(newCentres))]
    centres[, !is.nan(colSums(newCentres))] <- 
     newCentres[, !is.nan(colSums(newCentres))]
    
    if(interPlot) plot(rasterTemp)
    
  }
  
#--End of function------------------------------------------------------------
  
  return(list("Raster" = rasterTemp, "Centers" = centres))
}
