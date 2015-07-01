EasyMosaic <- function(dataIn = choose.files(), resolve = "",
 fileOut = TempRasterName(), silent = TRUE){
#Function to easily load, filter and mosaic a set of rasters.
#Currently requires that all rasters have the same projection, resolution ect.
#
#Args:
#  dataIn: A set of files to mosaic, defaults to choose.files(). Passed
#   through RasterLoad.
#  resample: Boolean value - if any inputs have different resolutions
#   ect. this will cause them to be re-sampled to the most common input.
#   This can and will change data values, as well as take a lot of computation
#   time. Generally un-advised but can be useful to produce an output from
#   irregular inputs.
#  resolve: String to choose how to resolve any resolution differences:
#    "resample" - Resample all rasters to the most common input. Can take a
#     significant quantity of time.
#    "multiple" - Produces a list of mosaiced rasters, one for each different
#     resolution in.
#    All other option will return only the most common resolution. In the case
#     of two being equally common, will pick the first in the input list.
#  fileOut:  Name to write file to, defaults to temporary file.
#  silent: Should details of the classification be output as it works?
#
#Returns
#  A mosaic of all of the inputs, with non-conforming inputs either excluded
#   or resampled.

  library("REORS")
  
  #Sub-function to mosaic all rasters in a list.
  #Is called at several points later. Defined as a function as there's several
   #variants of the end result.
  mosaicList <- function(rasterIn){
    #Add argument names
    names(rasterIn)[1] <- "x"
    names(rasterIn)[2] <- "y"
    rasterIn$fun <- mean    #Maybe offer a choice of function here?
    rasterIn$filename <- fileOut
    rasterIn$format <- "GTiff"
    
    rasterOut <- do.call(mosaic, args = rasterIn)
    return(rasterOut)
  }
  
  if(!silent) cat("Creating mosaic.\n")
  
  rasterIn <- list()
  resMat <- matrix(ncol = 2, nrow = length(dataIn))
  
  if(!silent) cat("\tLoading data.\n")
  
  for(i in 1:length(dataIn)){
    rasterIn[[i]] <- RasterLoad(dataIn[i], retForm = "stack")                 #Swap around rasterIN & dataIn
    resMat[i, ] <- res(rasterIn[[i]])
  }
  
#--Resolve resolution conflicts-----------------------------------------------
  if(nrow(unique(resMat)) > 1){
    if(!silent) cat(sprintf("\t%s different resolutions present.",
     nrow(unique(resMat))))
    
    #List resolutions and which is inputs belong to the most common one.
    resCount <- cbind(unique(resMat), 0)
    for(i in 1:nrow(resCount)){
      resCount[i, 3] <- sum(rowSums(resMat == t(resCount[, 1:2])) == 2)
    }
    resCount <- resCount[order(resCount[, 3], decreasing = TRUE)]
    mostCommon <- rowSums(resMat == t(resCount[1, 1:2])) == 2
    
    #Resample to the most common resolution
    if(resolve == "resample"){                                                             #Untested
      if(!silent) cat(" Resampling all layers to most common.\n")
      for(i in which(!mostCommon)){
        rasterIn[i] <- projectRaster(
         from = rasterIn[[i]],
         to = raster(
          crs = crs(rasterIn[[i]]),
          ext = extent(rasterIn[[i]]),
          resolution = resCount
         )
        )
      }
      rasterOut <- mosaicList(rasterIn)
    #Create a mosaic for each resolution
    } else if(resolve == "multiple") {
      if(!silent) cat(" Creating output rasters for each resolution.\n")
      rasterOut <- list()
      for(i in 1:nrow(unique(resMat))){
        rasterOut[i] <- mosaicList(rasterIn[#Pick current resolution here.])
      }
    #Just use the most common resolution
    } else {
      if(!silent) cat(" Using only the most common resolution.\n")
      rasterIn <- rasterIn[which(mostCommon)]
      rasterOut <- mosaicList(rasterIn)
    }
  }
  
  
  
  return(rasterOut)
}
