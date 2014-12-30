FocalAngle <- function(rasterIn, kernelSize, outName = tempfile()){
#Calculates the standard deviations of the angles between the central cell of
# a kernel and all other cells. Something of a precursor to calculating 
# topographic openness.
#
#IMPORTANT NOTE: Elevation values & cell resolution must be in the same units
#
#Requires: WMat, RasterLoad
#
#Args:
#  rasterIn: The raster file to use in calculation, passed through RasterLoad,
#   though only the first layer is taken at the present time.
#  kernelSize: The size of kernel to use in focal calculations.
#  outName: Name to be used in output, defaults to a temporary file.
#
#Returns:
#  The RasterLayer containing calculated angles, also saved to disk.
  
  library("raster")
  
#--Create the matrix to run over the data-------------------------------------

  mUse <- WMat(kernelSize)
  cent <- c(ceiling(nrow(mUse) / 2), ceiling(ncol(mUse) / 2))
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  rasterIn <- raster(rasterIn, layer = 1)

  
  mDist <- mUse
  for(i in 1:nrow(mDist)){
    for(j in 1:ncol(mDist)){
      mDist[i, j] <- mDist[i, j] * 
       sqrt( ((cent[1] - i) * res(rasterIn)[1]) ^ 2 +
        ((cent[2] - j) * res(rasterIn)[1]) ^ 2)
    }
  }
  mDist[cent[1], cent[2]] <- NA
  
#--Calculate values-----------------------------------------------------------
  
  ret <- focal(
   x = rasterIn,
   w = mUse,
   fun = function(x){
     if(sum(is.na(x)) == sum(is.na(mUse))){
       sd(tan(x / mDist), na.rm=TRUE)
     } else return(NA)
   },
   filename = outName,
   format = "GTiff",
   overwrite = TRUE
  )
  
  return(ret)
}
