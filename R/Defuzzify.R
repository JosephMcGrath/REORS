Defuzzify <- function(rasterIn, meth = "max", opt = NULL,
 fileOut = tempfile(pattern = "REORS"), silent = TRUE){
#Converts fuzzy memberships into crisp memberships
#
#Requires: RasterShell
#
#Args:
#  rasterIn: The raster* object containing fuzzy membership values, should
#   have one layer for each class.
#  meth: The method used for defuzzification, current options are:
#   -"max": The class with the maximum membership.
#   -"th" : The class with the maximum membership, so long as it's above a
#     threshold (set with opt). Otherwise put into "undetermined" class.
#  opt: Options for the method chosen, for each meth argument:
#   -"max": No options, argument is ignored.
#   -"th" : The minimum membership value allowed.
#  fileOut: Name to write file to, defaults to temporary file.
#  silent: Should details of the classification be output as it works?
#
#Returns:
#  A single-layer raster of crisp memberships, also written to disk.
  
  library("raster")
  library("REORS")
  
  
  blocks <- blockSize(rasterIn)
  rasterOut <- RasterShell(rasterIn, 1)
  
  rasterTemp <- writeStart(rasterOut, filename = fileOut,
   format = "GTiff", overwrite = TRUE
  )
  
  if(!silent) cat("Defuzzifying raster.\n")
  
#--Take whichever membership value is highest---------------------------------
  if(meth = "max"){
    for(i in 1:blocks$n){
      if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
       i, blocks$n, round(i / blocks$n * 100)))
      
      tempValue <- getValues(
       rasterIn,
       row = blocks$row[i],
       nrow = blocks$nrow[i]
      )
      
      crispMemb <- rep(NA, nrow(tempValue))
      
      for(j in 1:nrow(tempValue)){
        crispMemb[j] <- order(tempValue[j, ], decreasing = TRUE)[1]
      }
      
      rasterTemp <- writeValues(
       x = rasterOut,
       v = crispMemb,
       start = blocks$row[i]
      )
    }
  }
  
#--Require membership values to be above a threshold--------------------------
  if(meth = "th"){
     unDet <- nlayers(rasterIn) + 1
     if(is.null(opt)){
       rasterOut <- writeStop(rasterOut)
       stop("Minimum thresold must be set with \"opt\".\n")
     }
     
     for(i in 1:blocks$n){
      if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
       i, blocks$n, round(i / blocks$n * 100)))
      
      tempValue <- getValues(
       rasterIn,
       row = blocks$row[i],
       nrow = blocks$nrow[i]
      )
      
      crispMemb <- rep(NA, nrow(tempValue))
      
      for(j in 1:nrow(tempValue)){
        crispMemb[j] <- order(tempValue[j, ], decreasing = TRUE)[1]
        if(tempValue[j, crispMemb[j]] < opt){
          crispMemb[j] <- unDet
        }
      }
      
      rasterTemp <- writeValues(
       x = rasterOut,
       v = crispMemb,
       start = blocks$row[i]
      )
    }
  }

  rasterOut <- writeStop(rasterOut)
  
  return(rasterOut)
}
