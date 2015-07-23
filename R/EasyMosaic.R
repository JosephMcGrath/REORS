EasyMosaic <- function(rasterIn = choose.files(), resolve = "", sumFun = mean,
 fileOut = TempRasterName(), silent = TRUE){
#Function to easily load, filter and mosaic a set of rasters.
#Currently requires that all rasters have the same projection, resolution ect.
#
#Args:
#  rasterIn: A set of files to mosaic, defaults to choose.files(). Passed
#   through RasterLoad.
#  resample: Boolean value - if any inputs have different resolutions
#   ect. this will cause them to be re-sampled to the most common input.
#   This can and will change data values, as well as take a lot of computation
#   time. Generally un-advised but can be useful to produce an output from
#   irregular inputs.
#  resolve: String to choose how to resolve any resolution differences:
#    "resample" - Resample all rasters to the most common input. Can take a
#     significant quantity of time. Can be quite processor intensive too.
#    "multiple" - Produces a list of mosaiced rasters, one for each different
#     resolution in.
#    All other option will return only the most common resolution. In the case
#     of two being equally common, will pick the first in the input list.
#  sumFun: Function to be passed to mosaic function (for resolving duplicates)
#  fileOut:  Name to write file to, defaults to temporary file.
#  silent: Should details of the classification be output as it works?
#
#Returns
#  A mosaic of all of the inputs, with non-conforming inputs either excluded
#   or resampled.
#
#To do:
#  When resampling, currently 

  library("REORS")
  
  #Sub-function to mosaic all rasters in a list.
  mosaicList <- function(tempIn, fun = sumFun, fileNameOut = fileOut){
    names(tempIn)[1] <- "x"
    names(tempIn)[2] <- "y"
    
    tempIn$fun <- sumFun
    tempIn$filename <- fileNameOut
    tempIn$format <- "GTiff"
    
    tempOut <- do.call(mosaic, args = tempIn)
    return(tempOut)
  }
  
  if(!silent) cat("Creating mosaic:\n")
  
  #Also account for differing crs & origin
  dataIn <- list()
  resMat <- matrix(ncol = 2, nrow = length(rasterIn))
  
  if(!silent) cat("\tLoading data.\n")
  
  for(i in 1:length(rasterIn)){
    dataIn[[i]] <- RasterLoad(rasterIn[i], retForm = "stack")
    resMat[i, 1:2] <- res(dataIn[[i]])
    #resMat[i, 3] <- crs(dataIn[[i]])
  }
  
#--Resolve resolution conflicts-----------------------------------------------
  if(nrow(unique(resMat)) > 1){
    if(!silent) cat(sprintf("\t%s different resolutions present.",
     nrow(unique(resMat))))
    
    #List resolutions and which is inputs belong to the most common one.
    resCount <- cbind(unique(resMat), 0)
    for(i in 1:nrow(resCount)){
      resCount[i, 3] <- sum(resMat[, 1] == resCount[i, 1] & resMat[, 2] == resCount[i, 2])
    }
    resCount <- resCount[order(resCount[, 3], decreasing = TRUE), ]
    mostCommon <- resMat[, 1] == resCount[1, 1] & resMat[, 2] == resCount[1, 2]
    
    protoType <- dataIn[mostCommon][[1]]
    
    #Resample to the most common resolution
    if(resolve == "resample"){
      if(!silent) cat(" Resampling all layers to most common.\n")
      for(i in which(!mostCommon)){
        if(!silent){
          cat(sprintf("\t\tResampling raster %s of %s\n", i, sum(!mostCommon)))
        }
        
        if(crs(dataIn[[i]]) == crs(protoType)){
          dataIn[i] <- resample(
           x = dataIn[[i]],
           y = raster(
            crs = crs(dataIn[[i]]),
            ext = alignExtent(extent(dataIn[[i]]), protoType),
            resolution = resCount
           ),
           filename = TempRasterName("int")
        )
        } else {
          dataIn[i] <- projectRaster(
           from = dataIn[[i]],
           to = raster(
            crs = crs(protoType),
            ext = alignExtent(extent(dataIn[[i]]), protoType),
            resolution = resCount
           ),
           filename = TempRasterName("int")
          )
        }
      }
      
      rasterOut <- mosaicList(dataIn)
    #Create a mosaic for each resolution                                      #To Do - File names out (append 1,2,3 ect.)
    } else if(resolve == "multiple") {
      if(!silent) cat(" Creating output rasters for each resolution.\n")
      rasterOut <- list()
      for(i in 1:nrow(resCount)){
        if(!silent){
          cat(sprintf("\t\tCreating mosaic %s of %s", i, nrow(resCount)))
        }
        print(dataIn[resMat[, 1] == resCount[i, 1] & resMat[, 2] == resCount[i, 2]])
        #rasterOut[i] <- mosaicList(["Get List of correct rasters here"], fileNameOut = sprintf("%s_%s", fileOut, i)
      }
    #Just use the most common resolution
    } else {
      if(!silent) cat(" Using only the most common resolution.\n")
      dataIn <- dataIn[which(mostCommon)]
      rasterOut <- mosaicList(dataIn)
    }
  } else {
    rasterOut <- mosaicList(dataIn)
  }
  
  return(rasterOut)
}
tempResults <- EasyMosaic(resolve = "resample", silent = FALSE)
plot(tempResults)
