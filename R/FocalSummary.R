FocalSummary <- function(rasterIn, kernelSize, sumFun,
 fileOut = tempfile(pattern = "REORS")){
#Calculates how high above the surrounding area each cell is.
#
#Requires: WMat, RasterLoad
#
#Hypsometric integral source: 
# Pike,R.J., Wilson,S.E. (1971). 
# Elevation-relief ratio, hypsometric integral, and geomorphic area-altitude
# analysis. Geological Society of America Bulletin, 82, 1079-1084
#
#Args:
#  rasterIn: The raster file to use in calculation, passed through RasterLoad
#   though only the first layer is taken at the present time.
#  kernelSize: The size of kernel to use
#  sumFun: A function used to summarise the kernel e.g. min, of max.
#   Will only use the first result of a function if it returns are multiple.
#   Special cases are input as strings:
#    "range" for the difference between max and min values (e.g. for relief).
#    "hyps" for hypsometric integral.
#    "AMin" for the difference between centre and minimum of the window.
#    "BMax" for the difference between maximum and centre of the window.
#  fileOut: Name to be used in output, defaults to temporary file.
#
#Returns:
#  A RasterLayer containing the summarised values.
  
  library("raster")
  library("REORS")
  
  mUse <- WMat(kernelSize)
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  rasterIn <- raster(rasterIn, layer = 1)
  
  if(!is.function(sumFun)){
    sumFun <- tolower(sumFun)
    #Currently writing to a temp object to avoid conflicts here
    if(sumFun == "range") sumTemp <- function(x) return(max(x) - min(x))
    if(sumFun == "hyps"){
      sumTemp <- function(x) return((mean(x) - min(x)) / (max(x) - min(x)))
    }
    if(sumFun == "amin"){
      cent <- ceiling(length(mUse) / 2)
      sumTemp <- function(x) return(x[cent] - min(x))
    }
    if(sumFun == "bmax"){
      cent <- ceiling(length(mUse) / 2)
      sumTemp <- function(x) return(max(x) - x[cent])
    }
    if(exists("sumTemp")){
      sumFun <- sumTemp
    } else stop("Invalid input")
  }
  
  rasterOut <- focal(
   x = rasterIn,
   w = mUse,
   fun = function(x){
     if(sum(is.na(x)) == sum(is.na(mUse))){
       return(sumFun(x[!is.na(x)])[1])
     } else return(NA)
   },
   filename = fileOut,
   format = "GTiff",
   overwrite = TRUE
  )
  
  return(rasterOut)
}
