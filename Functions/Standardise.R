Standardise <- function(rasterIn, minMax = c(0, 1), intLock = FALSE,
 fileName = tempfile(), silent = TRUE){
#Takes all values in a given raster and scales them to be between 0 and 1     <--Add 8bit signed functionality? Or more general minMax values?
#
#Args:
#  rasterIn: the raster file to be normalised
#  minMax: The minimum/maximum values to stretch to.
#  intLock: Boolean; should it round to the nearest whole number?
#  fileName: the location to save the result, if absent will save as a
#   temporary file.
#  silent: should information about progress be returned?
#
#Returns:
#  A raster file with the results of normalisation, also saves a file output
  
  library("raster")

#--Set up and detect data type to use-----------------------------------------
  ret <- rasterIn
  rasterIn <- setMinMax(rasterIn)
  mv <- list(minValue(rasterIn), maxValue(rasterIn), maxValue(rasterIn) - minValue(rasterIn))
  blocks <- blockSize(rasterIn)
  
  if(!silent) print(rasterIn)
  
  if(intLock){
    dataTypeD <- sprintf(
     "INT%s%s",
     max(c(ceiling(log(max(abs(minMax)), base = 2)/ 8), 1)),
     if(minMax[2] < 0) "S" else "U"
    )
  } else dataTypeD <- "FLT8S"
  
  ret <- writeStart(x = ret, filename = fileName, format = "GTiff",
   datatype = dataTypeD, overwrite = TRUE
  )
  
#--Perform the calculations---------------------------------------------------
  for(j in 1:blocks$n){
    if(!silent) cat(sprintf("Processing block %s of %s\n", j, blocks$n))
    
    tempValues <- as.matrix(getValues(
     rasterIn,
     row = blocks$row[j],
     nrow = blocks$nrow[j]
    ))
    
    #Not using apply currently to access the min/max values properly          <--See if it's more efficient to change to apply if I can
    if(intLock){
      for(i in 1:ncol(tempValues)){
        tempValues[, i] <- round((tempValues[, i] - mv[[1]][i]) /
         (mv[[3]][i]) * (minMax[2] - minMax[1]) + minMax[1])
      }
    } else {
      for(i in 1:ncol(tempValues)){
        tempValues[, i] <- (tempValues[, i] - mv[[1]][i]) /
         (mv[[3]][i]) * (minMax[2] - minMax[1]) + minMax[1]
      }
    }
    
    ret <- writeValues(
     x = ret,
     v = tempValues,
     start = blocks$row[j]
    )
  }
  
  ret <- writeStop(ret)
  
  if(!silent) print(ret)
  
  return(ret)
}
