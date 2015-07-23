CMeans <- function(rasterIn, nCentres = 10, its = 1, weight = 1, fuzz = 2,
 init = "lin", breakCon = 0.01, standIn = FALSE, distM = "euc",
 fileOut = TempRasterName(), silent = TRUE, retCent = FALSE){
#Uses the fuzzy c-means algorithm, with some additional customisation
# available in terms of behaviour. Iteratively assigns fuzzy membership values
# to pixels in an attempt to produce the best classification.
#
#Heavily modelled off the KMeans function
#Weighing is currently handled as values are pulled in, probably less
# efficient overall, but is simpler this way for now. Might also add some
# flexibility on weighting, though that is somewhat tenuous.
#
#Requires: RasterLoad, RasterShell, Standardise
#
#To do:
# Add a calculation of the objective function as a measure of quality?
# Split main iteration and final write? Could save more time.
#
#Args:
#  rasterIn: Raster objects to be classified. Ran through RasterLoad.
#  nCentres: Number of clusters to split the data into. Ignored if "init" is
#   uses pre-set centres.
#  its: Maximum number of iterations to run the algorithm for.
#  weight: The weights to apply to each layer (higher weight means an greater
#   importance to clustering, can be integers or decimals. Must be a single
#   values or have length equal to the number of input layers.
#  fuzz: The fuzzification parameter (often called m), higher values means
#   membership values are drawn from a more diverse range of pixels. Cannot be
#   1 by definition, values below 1 will produces weird results.
#  init: How should the clusters be initialised? Current methods are:
#    - "lin": Linearly, from the minimum value in each layer, to the maximum.
#    - "rand": Randomly within the minimum/maximum available values.
#    - Matrix of centres, one column for each centre, one row for each layer.
#  breakCon: How little variation between iterations will break the loop early
#   calculated as an average per variable, assuming variables are between 0-1.
#   should be possible to linearly multiply out for larger ranges.
#  standIn: Should the data be locked between 0 and 1 before classification?
#  distM: Distance measure to calculate memberships:
#    - "euc": euclidean distance
#    - "man": Manhattan distance
#    - "eu2": squared euclidean distance
#  fileOut: Name to write file to, defaults to temporary file.
#  silent: Should details of the classification be output as it works?
#  retCent: If cluster centres should be returned. Gives a list containing
#   the clustered results, as well as a matrix of cluster centres.
#
#Returns:
#  Depending on retCent, either a Raster* object of the classified results, or a
#   list containing two items:
#     -Raster:  The classified raster file.
#     -Centres: The centres used to classify the raster file.

#--Set up and build the initial centres---------------------------------------
  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(length(weight) != 1 & length(weight) != nlayers(rasterIn)){
    stop("Weights must have values for each layer of input or a single value")
  }
  if(fuzz == 1) stop("Fuzzification parameter may not be 1.")
  
  centres <- matrix(ncol = nCentres, nrow = nlayers(rasterIn))
  colnames(centres) <- sprintf("Clust %s", 1:ncol(centres))
  rownames(centres) <- names(rasterIn)
  
  if(standIn) rasterIn <- Standardise(rasterIn, c(0, 1), silent = silent)
  
  if(is.na(max(maxValue(rasterIn)))) rasterIn <- setMinMax(rasterIn)
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- RasterShell(rasterIn, nCentres)
  
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
      for(j in 1:ncol(centres)){
        centres[i, j] <- runif(
         1,
         minValue(rasterIn)[i],
         maxValue(rasterIn)[i]
        )
      }
    }
  } else stop("Invalid initialisation method.\n")
  
  if(!silent) cat("Beginning fuzzy c-means clustering:\n")
  
  if(!silent){
    cat("Initial centres (pre-weighting):\n")
    print(centres)
  }
  
  #Apply weights
  centres <- centres * weight
  
#--Set up distance measure of choice------------------------------------------
  if(distM == "euc"){
    distM <-  function(x, y) return(sqrt(rowSums((x - y) ^ 2)))
  } else if(distM == "man"){
    distM <- function(x, y) return(rowSums(abs(x - y)))
  } else if(distM == "eu2"){
    distM <- function(x, y) return(rowSums((x - y) ^ 2))
  } else stop("Invalid distance measure")
  
#--Run the algorithm for each iteration---------------------------------------
  for(i in 1:its){
    
    #Break to write on the last iteration.
    #This method also catches single iteration runs
    if(i >= its) break
    
    if(!silent) cat(sprintf("Beginning iteration %s of %s\n", i, its))
    
    tempCentres <- centres * 0
    tempCentres2 <- centres * 0
    classCount <- rep(0, ncol(centres))
    
#--Calculate memberships and new centres--------------------------------------
    for(j in 1:blocks$n){
      if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
       j, blocks$n, round(j / blocks$n * 100)))
      
      tempValue <- getValues(
       rasterIn,
       row = blocks$row[j],
       nrow = blocks$nrow[j]
      )
      
      #Calculate membership values.
      membTemp <- matrix(nrow = nrow(tempValue), ncol = ncol(centres))
      
      for(k in 1:ncol(membTemp)){
        membTemp[, k] <- distM(tempValue * weight, matrix(centres[, k],
         ncol = length(centres[, k]), nrow = nrow(tempValue), byrow = TRUE))
      }
      
      #Use a temporary copy, so it doesn't use the new values in later columns
      membTemp2 <- membTemp
      
      fP <- (2 / (fuzz - 1))
      for(k in 1:ncol(membTemp)){
        membTemp2[, k] <- 1 / rowSums(membTemp[, k] / membTemp) ^ fP
      }
      
      membTemp <- membTemp2
      
      membTemp[is.nan(membTemp)] <- 1
      
      for(k in 1:ncol(membTemp)){
        classCount[k] <- classCount[k] + sum(membTemp[!is.na(membTemp[, k]),
         k] ^ fuzz)
        
        tempCentres2[, k] <- tempCentres2[, k] + colSums((membTemp[, k] ^ fuzz
         ) * tempValue, na.rm = TRUE)
      }
      
    }
    
    newCentres <- t(t(tempCentres2) / classCount)
    
    #is.nan() used to compensate for clusters with 0 pixels in the cluster
    diffSince <- (sum(abs(centres[, !is.nan(colSums(newCentres))] - 
     newCentres[, !is.nan(colSums(newCentres))])) / 
     (ncol(centres) * mean(weight))) /
     #Divide by the maximum distance in populated feature-space.
     sqrt(sum((maxValue(rasterIn) - minValue(rasterIn)) ^ 2))
    if(!silent) cat(sprintf("%s difference since last iteration.\n",
     round(diffSince, 3)))
    
    
    if(diffSince <= breakCon) {
      #Breaking before re-assigning centres to avoid adding another iteration
      # when writing converged output.
      if(!silent) cat("Converged, breaking loop.\n")
      break
    }
    
    #Keeping centres with no changes as they are.
    #centres <- newCentres[, !is.nan(colSums(newCentres))]
    centres[, !is.nan(colSums(newCentres))] <- 
     newCentres[, !is.nan(colSums(newCentres))]
    
  }
  
#--Writing iteration----------------------------------------------------------
  if(!silent){
    cat(sprintf("Writing final result to %s\n", fileOut))
  }

  rasterTemp <- writeStart(rasterTemp, filename = fileOut,
   format = "GTiff", overwrite = TRUE
  )
  
  tempCentres <- centres * 0
  tempCentres2 <- centres * 0
  
  classCount <- rep(0, ncol(centres))
  
  for(j in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     j, blocks$n, round(j / blocks$n * 100)))
    
    tempValue <- getValues(
     rasterIn,
     row = blocks$row[j],
     nrow = blocks$nrow[j]
    )
    
    #Calculate membership values.
    membTemp <- matrix(nrow = nrow(tempValue), ncol = ncol(centres))
    
    for(k in 1:ncol(membTemp)){
      membTemp[, k] <- distM(tempValue * weight, matrix(centres[, k],
       ncol = length(centres[, k]), nrow = nrow(tempValue), byrow = TRUE))
    }
    
    #Use a temporary copy, so it doesn't use the new values in later columns
    membTemp2 <- membTemp
    
    fP <- (2 / (fuzz - 1))
    for(k in 1:ncol(membTemp)){
      membTemp2[, k] <- 1 / rowSums(membTemp[, k] / membTemp) ^ fP
    }
    
    membTemp <- membTemp2
    
    membTemp[is.nan(membTemp)] <- 1
    
    rasterTemp <- writeValues(
     x = rasterTemp,
     v = membTemp,
     start = blocks$row[j]
    )
    
    for(k in 1:ncol(membTemp)){
      classCount[k] <- classCount[k] + sum(membTemp[!is.na(membTemp[, k]),
       k] ^ fuzz)
      
      tempCentres2[, k] <- tempCentres2[, k] + colSums((membTemp[, k] ^ fuzz
       ) * tempValue, na.rm = TRUE)
    }
    
  }
  
  rasterTemp <- writeStop(rasterTemp)
  
  newCentres <- t(t(tempCentres2) / classCount)
    
  #is.nan() used to compensate for clusters with 0 pixels in the cluster
  diffSince <- (sum(abs(centres[, !is.nan(colSums(newCentres))] - 
   newCentres[, !is.nan(colSums(newCentres))])) / 
   (ncol(centres) * mean(weight))) /
   #Divide by the maximum distance in populated feature-space.
   sqrt(sum((maxValue(rasterIn) - minValue(rasterIn)) ^ 2))
  
  #Keeping centres with no changes as they are.
  #centres <- newCentres[, !is.nan(colSums(newCentres))]
  centres[, !is.nan(colSums(newCentres))] <- 
   newCentres[, !is.nan(colSums(newCentres))]

#If any clusters are still empty, set them to NA to avoid
 #implying they have data points attached to them.
  for(i in 1:ncol(centres)){
    if(classCount[i] == 0){
      centres[, i] <- NA
    }
  }
  
#--End of function------------------------------------------------------------
  if(!silent) cat("\n")
  if(retCent){
    ret <- list("Raster" = rasterTemp, "Centres" = centres / weight)
  }else{
    ret <- rasterTemp
  }
  return(ret)
}
