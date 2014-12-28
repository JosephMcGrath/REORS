KMeans <- function(rasterIn, nCentres = 10, fileName = tempfile(), itts = 1,
 breakCon = 5, distM = "euc", silent = TRUE, interPlot = FALSE){
#Standard k-means clustering algorithm.
#Somewhat of a work in progress, data still needs to be standardised before
# input to avoid artificial weighting and weighting of inputs still
# needs to be added.
#
#Args:
#  rasterIn: Name of the image file to classify. Maybe run it through a
#   cleaning function first?
#  nCentres: How many clusters should the data be split into? Empty clusters
#   are deleted as the algorithm progresses - may change this later.
#  fileName: Name to write file to, defaults to temporary file.
#  itts: Maximum number of iterations to run the algorithm for.
#  breakCon: How little variation between iterations will break the loop early
#   not very well implemented yet - just sum of distances.
#  distM: Distance measure to calculate memberships:
#    "euc" = euclidean distance
#    "man" = Manhattan distance
#    "eu2" = squared euclidean distance
#  silent: Should details of the classification be output as it works?
#  interPlot: Should the classification be plotted each iteration?
#
#Returns:
#  Image (also written to disk) with classes represented by numerical values.

#--Set up and build the initial centres---------------------------------------
  library("raster")
  
  #Standardisation step here
  
  if(is.na(maxValue(rasterIn)[1])) rasterIn <- setMinMax(rasterIn)
  
  blocks <- blockSize(rasterIn)
  centres <- matrix(ncol = nCentres, nrow = nlayers(rasterIn))
  rasterTemp <- raster(rasterIn)
  
  for(i in 1:nlayers(rasterIn)){
    centres[i, ] <- seq(
     from = minValue(rasterIn)[i],
     to = maxValue(rasterIn)[i],
     length.out = nCentres
    )
  }
  
  if(!silent) print(centres)
  
#--Set up distance measure of choice------------------------------------------
  if(distM == "euc"){
    distM <-  function(x, y) return(sqrt(colSums((x - y) ^ 2)))
  } else if(distM == "man"){
    distM <- function(x, y) return(colSums(abs(x - y)))
  } else if("eu2"){
    distM <- function(x, y) return(colSums((x - y) ^ 2))
  } else stop("Invalid distance measure")
  
#--Run the algorithm for each iteration---------------------------------------
  for(i in 1:itts){
    
    if(!silent) cat(sprintf("Beginning iteration %s of %s\n", i, itts))
    
    tempCentres <- centres * 0
    classCount <- rep(0, ncol(centres))
    
    rasterTemp <- writeStart(rasterTemp, filename = fileName,
     format = "GTiff", overwrite = TRUE
    )
    
#--Calculate memberships and new centres--------------------------------------
    for(j in 1:blocks$n){
      tempTime <- Sys.time()
      if(!silent) cat(sprintf("\tProcessing block %s of %s\n", j, blocks$n))
      
      tempValue <- getValues(
       rasterIn,
       row = blocks$row[j],
       nrow = blocks$nrow[j]
      )
      
      tempClass <- apply(
       X = tempValue,
       MARGIN = 1,
       FUN = function(x){
         tmp <- distM(x, centres)
         return(sum(1:ncol(centres) * 
          (tmp == min(tmp))))
       }
      )
    
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
    if(!silent) cat(sprintf("%s difference since last iteration.\n",
     sum(abs(centres[, !is.nan(colSums(newCentres))] - 
      newCentres[, !is.nan(colSums(newCentres))])))
    )
    if(sum(abs(centres[, !is.nan(colSums(newCentres))] - 
     newCentres[, !is.nan(colSums(newCentres))]))
     < breakCon) {
      if(!silent) cat("Converged, breaking loop.\n")
      break
    }
    
  #Currently deleting empty cluster, could leave them as previous otherwise.
    centres <- newCentres[, !is.nan(colSums(newCentres))]
    #centres[, !is.nan(colSums(newCentres))] <- newCentres
    
    if(interPlot) plot(rasterTemp)
    
    print(Sys.time() - tempTime)
    
  }
  
#--End of function------------------------------------------------------------
  
  return(rasterTemp)
}
