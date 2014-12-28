library("raster")
rasterIn <- brick()
nCentre <- 4
fileOut <- tempfile()
crispOut <- tempfile()
itts <- 

#FCM <- function(rasterIn, nCentre = 4)
#Description
#
#Args:
#  rasterIn:
#  nCentre:
#  membershipOut: 
#  crispOut: 
#  itts: 
#
#Returns:
#  A rasterBrick that contains membership values for each pixel.

  library("raster")
  blocks <- blockSize(rasterIn)
  centres <- matrix(ncol = nCentres, nrow = nlayers(rasterIn))
  rasterTemp <- brick(rasterIn, values = FALSE)
  
  for(i in 1:nlayers(rasterIn)){  #<--Transpose this?
    centres[i, ] <- seq(
     from = minValue(rasterIn)[i],
     to = maxValue(rasterIn)[i],
     length.out = nCentres
    )
  }
  
  for(i in 1:itts){
    #Set up for the iteration
    rasterTemp <- writeStart(rasterTemp, filename = fileName,
     overwrite = TRUE
    )
    
    centreTemp <- list(
      num = matrix(ncol = ncol(centres), nrow = nrow(centres)),
      dem = matrix(ncol = ncol(centres), nrow = nrow(centres))
    )
    for(j in 1:blocks$n){
      #Load the current block
      tempValue <- getValues(
       rasterIn,
       row = blocks$row[j],
       nrow = blocks$nrow[j]
      )
      
      tempMem <- getValues(
       rasterTemp,
       row = blocks$row[j],
       nrow = blocks$nrow[j]
      )
      
      #Calculate memberships
      tempClass <- apply(
       X = tempValue,
       MARGIN = 1,
       FUN = function(x){
         tmp <- distM(x, centres)
         return(sum(1:ncol(centres) * 
          (tmp == min(tmp))))
       }
      )
      
      #Write values to disk
      writeValues(
       x = rasterTemp,
       v = tempClass,
       start = blocks$row[j]
      )
    }
  
  rasterTemp <- writeStop(rasterTemp) 
  }
  
#  return(list(crispClusters = crispOut, membershipValues = rasterTemp,
#   centres = centres)
#  )
#}