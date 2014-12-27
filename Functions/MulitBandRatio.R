MulitBandRatio <- function(rasterIn, bands, fileName = tempfile()){
#Simultaneously calculates multiple band ratios for a multi-band raster input
#
#Args:
#  rasterIn: the multi-layered raster to use
#  bands: A list containing pairs of bands to take as ratios
#  fileName: the name of the file to write out, defaults to a temporary file
#Returns:
#  A rasterBrick of the various ratios, in order.
  
  library("raster")
  
  inTest <- FALSE
  if(length(bands) < 2)inTest <- TRUE
  for(i in 1:length(bands)) if(length(bands[[i]]) != 2) inTest <- TRUE
  if(inTest) stop("Invalid band specifications")
  
  blocks <- blockSize(rasterIn)
  rasterTemp <- brick(rasterIn, nl = length(bands))
  rasterTemp <- writeStart(rasterTemp, filename = fileName, format = "GTiff",
   overwrite = TRUE)
  
  for(i in 1:blocks$n){
    tempValues <- getValues(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i]
    )
    
    rasterTemp <- writeValues(
     x = rasterTemp,
     v = t(apply(
      X = tempValues,
      MARGIN = 1,
      FUN = function(x){
        ret <- c()
        for(j in 1:length(bands)){
          ret <- append(ret, x[[bands[[j]][1]]] / x[[bands[[j]][2]]])
        }
        return(ret)
      }
     )),
     start = blocks$row[i]
    )
    
  }
    
  rasterTemp <- writeStop(rasterTemp)

  return(rasterTemp)
}
