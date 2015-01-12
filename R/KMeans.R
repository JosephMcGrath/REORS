KMeans <- function(rasterIn, nCentres = 10, its = 1, weight = 1, init = "lin",
 fileOut = tempfile(pattern = "REORS"), breakCon = 0.01, standIn = FALSE,
 distM = "euc", randRe = FALSE, silent = TRUE, interPlot = FALSE){
#Standard k-means clustering algorithm.
#Weighing is currently handled as values are pulled in, probably less
# efficient overall, but is simpler this way for now. Might also add some
# flexibility on weighting, though that is somewhat tenuous.
#
#Requires: RasterLoad, RasterShell, Standardise
#
#ToDo:
#Could probably be sped up by replacing apply with matrix algebra. Pretty big
# overhaul though. Not quite sure it's possible without the for loop.
#Add option for pre-defined centres?
#
#Args:
#  rasterIn: Name of the image file to classify. Maybe run it through a
#   cleaning function first?
#  nCentres: How many clusters should the data be split into?
#  its: Maximum number of iterations to run the algorithm for.
#  weight: The weights to apply to each layer (higher weight means an greater
#   importance to clustering, can be integers or decimals. Must be a single
#   values or have length equal to the number of input layers.
#  init: How should the clusters be initialised? Current methods are:
#    - "lin": Linearly, from the minimum value in each layer, to the maximum.
#    - "rand": Randomly within the minimum/maximum available values.
#    - Matrix of centres, one column for each centre, one row for each layer.
#  fileOut: Name to write file to, defaults to temporary file.
#  breakCon: How little variation between iterations will break the loop early
#   calculated as an average per variable, assuming variables are between 0-1.
#  standIn: Should the data be locked between 0 and 1 before classification?
#  distM: Distance measure to calculate memberships:
#    - "euc": euclidean distance
#    - "man": Manhattan distance
#    - "eu2": squared euclidean distance
#  randRe: If a class is empty at the end of an iteration, should it be
#   re-assigned at random? Can disrupt existing clusters, but makes it more
#   likely that all clusters are populated. Higher "its" value recommended.
#  silent: Should details of the classification be output as it works?
#  interPlot: Should the classification be plotted each iteration?
#
#Returns:
#  List containing two items:
#    -Raster:  The classified raster file.
#    -Centres: The centres used to classify the raster file.

#--Set up and build the initial centres---------------------------------------
  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(length(weight) != 1 & length(weight) != nlayers(rasterIn)){
    stop("Weights must have values for each layer of input or a single value")
  }
  
  centres <- matrix(ncol = nCentres, nrow = nlayers(rasterIn))
  colnames(centres) <- sprintf("Clust %s", 1:ncol(centres))
  rownames(centres) <- names(rasterIn)
  
  if(silent){
    if(standIn) rasterIn <- Standardise(rasterIn, c(0, 1))
  } else {
    if(standIn) rasterIn <- Standardise(rasterIn, c(0, 1), silent = FALSE)
  }
  
  if(is.na(max(maxValue(rasterIn)))) rasterIn <- setMinMax(rasterIn)
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- RasterShell(rasterIn, 1)
  
  if(is.matrix(init)){
    if(ncol(init) == ncol(centres) & nrow(init) == nrow(centres)){
      centres <- init
    } else stop("Invalid matrix of centres provided.\n")
  } else if(init[1] == "lin"){
    for(i in 1:nlayers(rasterIn)){
      centres[i, ] <- seq(
       from = minValue(rasterIn)[i],
       to = maxValue(rasterIn)[i],
       length.out = nCentres
      )
    }
  } else if(init[1] == "rand"){
    for(i in 1:nlayers(rasterIn)){
      for(j in 1:nrow(centres)){
        centres[j, i] <- runif(
         1,
         minValue(rasterIn)[i],
         maxValue(rasterIn)[i]
        )
      }
    }
 } else stop("Invalid initialisation method.\n")
  
  
  if(!silent){
    cat("Initial centres (pre-weighting):\n")
    print(centres)
  }
  
  #Apply weights
  centres <- centres * weight
  
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
      if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
       j, blocks$n, round(j / blocks$n * 100)))
      
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
         colSums(matrix(tempValue[k == tempClass, ],
          ncol = nrow(centres)), na.rm = TRUE
         ) * weight
      }
      
    }
    
    rasterTemp <- writeStop(rasterTemp)
    newCentres <- t(t(tempCentres) / classCount)
    
    #is.nan() used to compensate for clusters with 0 pixels in the cluster
    diffSince <- sum(abs(centres[, !is.nan(colSums(newCentres))] - 
      newCentres[, !is.nan(colSums(newCentres))])) / 
      (ncol(centres) * mean(weight))
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
    
    if(randRe){
      for(j in 1:ncol(centres)){
        if(classCount[j] == 0){
          for(k in 1:nrow(centres)){
            centres[k, j] <- runif(
             1,
             minValue(rasterIn)[k],
             maxValue(rasterIn)[k]
            )
          }
        }
      }
    }
    
    if(interPlot) plot(rasterTemp, col = rainbow(nCentres))
    
  }
  
#If any clusters are still empty, set them to NA to avoid implying they have
 #data points attached to them.
  for(i in 1:ncol(centres)){
    if(classCount[i] == 0){
      centres[, i] <- NA
    }
  }
  
#--End of function------------------------------------------------------------
  
  return(list("Raster" = rasterTemp, "Centres" = centres / weight))
}
