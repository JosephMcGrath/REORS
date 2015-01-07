GaussianSmooth <- function(rasterIn, kernelSize,
 fileOut = tempfile(pattern = "REORS"), sig = 1, type = "circle"){
#Smooths the supplied raster using a Gaussian filter.
#Sigma value for calculating the Gaussian distribution is adjusted -  roughly
# calibrated to cover the entire kernel (not guaranteed to be exact).
#
#ToDo:
#  Merge with the WMat function, to give more flexibility.
#
#Requires: WMat, RasterLoad
#
#Args:
#  rasterIn: the raster file to be smoothed.
#  kernelSize: the size of kernel to smooth using.
#  fileOut: the name to be given to the resulting file.
#  sig: the value of sigma used in the calculation of the filter. Higher
#   values will give a more focused filter.
#  type: to be passed to WMat - the shape of kernel to use.
#
#Returns:
#  A RasterLayer containing the smoothed results.
  
  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
#Can only use the one layer of a raster here, defaulting to the first.
  rasterIn <- raster(rasterIn, layer = 1)
  
  if(round(kernelSize) %% 2 == 0){
    stop("Cannot have even number or rows/columns")
  }
  
  ret <- matrix(ncol = kernelSize, nrow = kernelSize)
  
  mid <- ceiling(ncol(ret) / 2)
  
#Equation is just pulled from the internet, not 100% sure how it works
  for(i in 1:ncol(ret)){
    for(j in 1:nrow(ret)){
      ret[i, j] <- (1 / (2 * pi * (sig ^ 2)) *
       exp(-1 * (((i - mid) / (0.2 * ncol(ret))) ^ 2 +
       ((j - mid) / (0.2 * nrow(ret))) ^ 2) / 2 * sig ^ 2))
    }
  }
  
  ret <- ret / sum(ret)
  ret <- ret * WMat(kernelSize, type = type)
  
  ret <- focal(
   x = rasterIn,
   w = ret,
   fun = function(x){
     if(sum(is.na(x)) == sum(is.na(ret))){
       return(sum(x, na.rm = TRUE))
     } else return(NA)
   },
   filename = fileOut,
   format = "GTiff",
   overwrite = TRUE
  )
  
  return(ret)
}
