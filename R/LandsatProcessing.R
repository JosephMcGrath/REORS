LandsatProcessing <- function(filePath, props = NULL,
 fileOut = paste0(c(filePath, "Processed"), collapse = "/"), silent = TRUE){
#Pulls in Landsat bands from single layers and stacks them together. The
# function assumes that the files are the only files in the set folder. Also
# assumes default naming scheme.
#
#To do:
# Comprehensively test out for different satellites. Done some testing,
#  but not on older satellites. May have issues confusing between TM and TM+
#
#Args:
#  filePath: The folder containing the bands to be combined.
#  props: Optional properties to modify the output to. Passes the argument
#   to projectRaster. If a Raster* object, passes to the "to" argument,
#   otherwise will be passed to the "crs" argument.
#  fileOut: The output location, including filename. Defaults to inside the
#   folder files are taken from.
#  silent: should the function work without progress reports?
#
#Returns:
#  The processed RasterBrick. Also writes a text file describing bands.

  library("raster")
  library("REORS")
  if(!silent) cat("Importing Landsat data.\n")
  
#--Get input images and organise them----------------------------------------
  if(!silent) cat("\tFinding and sorting layers.\n")
  
  toUse <- list.files(
   path = filePath,
   pattern = "TIF",
   full.names = TRUE
  )
  
  #Converts the text names into integers for proper sorting
  bandOrder <- c()
  for(i in strsplit(toUse, "_B")){
    bandOrder <- append(
     bandOrder,
     #Masks out bands without numerical names
     suppressWarnings(as.numeric(
      strsplit(strsplit(i[[2]], ".TIF")[[1]], "_")[[1]][[1]]
     ))
    )
  }
  
  #If there's multiple of one band (e.g. LS7's thermal). Take the first.
  if(length(bandOrder) != length(unique(bandOrder))){
    temp <- rep(NA, length(bandOrder))
    for(i in 1:length(bandOrder)){
      if(bandOrder[i] %in% temp){
        temp[i] <- NA
      } else {
        temp[i] <- bandOrder[i]
      }
    }
    bandOrder <- temp
  }
  
  toUse[!is.na(bandOrder)] <- toUse[
   !is.na(bandOrder)][order(bandOrder[!is.na(bandOrder)])]
  
#--Check that all layers can be stacked---------------------------------------
  #Something of a formality as there's a set input format.
  #Prevents the later steps breaking
  rasterTemp <- raster(toUse[1])
  
  for(i in 2:length(toUse)){
    if(compareRaster(rasterTemp, raster(toUse[i]), stopiffalse = FALSE) &
     !is.na(bandOrder[i])){
      rasterTemp <- stack(rasterTemp, raster(toUse[i]))
    } else {
      bandOrder[i] <- NA
    }
  }
  
#--Detect the band designations-----------------------------------------------
#Not 100% sure here, especially for older systems.

#By filename
  lsType1 <- as.numeric(substr(names(rasterTemp)[[1]], 3, 3))
  if(lsType1 > 1 & lsType1 <= 3){
    lsType1 <- 1
  } else if(lsType1 > 4 & lsType1 <= 5){
    lsType1 <- 4
  } else if(lsType1 == 7){
    lsType1 <- 7
  } else if(lsType1 == 8){
    lsType1 <- 8
  }
  
#By number of bands
  numBand <- sum(!is.na(bandOrder))
  if(numBand == 4){
    1
  } else if(numBand == 7){
    lsType2 <- 4
  } else if(numBand == 10){
    lsType2 <- 8
  }
  
  if(lsType1 == lsType2) lsType <- lsType1
  
  if(lsType == 1){
    bandNames <- c("Green", "Red", "", "")
  } else if(lsType == 4){
    bandNames <- c("Blue", "Green", "Red", "Near IR", "Short wave IR  1",
     "Thermal IR A", "Thermal IR B", "Short wave IR 2")
  } else if(lsType == 8){
    bandNames <- c("Costal", "Blue", "Green", "Red", "Near IR",
     "Short wave IR  1", "Short wave IR  1", "Cirrus", "Panchromatic",
     "Thermal IR 1", "Thermal IR 2"
    )
  }
  
  bandNames <- bandNames[!is.na(bandOrder)]
  
  write.table(
   x = cbind(1:nlayers(rasterTemp), names(rasterTemp), bandNames),
   file = sprintf("%s band descriptions.txt", fileOut),
   sep = "\t",
   col.names = FALSE,
   row.names = FALSE,
   quote = FALSE
  )
  
  if(!silent){
    cat("\tLayers used:\n")
    cat(sprintf("\t\t%s\n", names(rasterTemp)))
  }
  
#--Mask out all pixels without full set of values-----------------------------
  blocks <- blockSize(rasterTemp)
  rasterOut <- RasterShell(rasterTemp)
  
  rasterOut <- writeStart(rasterOut, filename = fileOut,
     format = "GTiff", overwrite = TRUE
  )
  
  if(!silent){ 
    cat(sprintf("\tStacking layers.\n\t\tWriting to %s.tif\n", fileOut))
  }
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\t\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
    
    tempValues <- getValues(
       rasterTemp,
       row = blocks$row[i],
       nrow = blocks$nrow[i]
    )
    
    for(j in 1:nrow(tempValues)){
      if(any(is.na(tempValues[j, ]), tempValues[j, ] == 0)){
        tempValues[j, ] <- NA
      }
    }
    
    rasterOut <- writeValues(
     x = rasterOut,
     v = tempValues,
     start = blocks$row[i]
    )
  }
  rasterOut <- writeStop(rasterOut)
  
#--Reproject if requested-----------------------------------------------------
  if(class(props)[[1]] %in% 
   c("RasterLayer", "RasterBrick", "RasterStack")){
    rasterOut <- projectRaster(
     from = rasterOut,
     to = props,
     filename = fileOut,
     format = "GTiff",
     overwrite = TRUE,
     datatype = sprintf("INT%sU", ceiling(round(log(max(maxValue(rasterOut))
      , 2)) / 8))
    )
  } else if(!is.null(props)) {
    rasterOut <- projectRaster(
     from = rasterOut,
     crs = props,
     filename = fileOut,
     format = "GTiff",
     overwrite = TRUE,
     datatype = sprintf("INT%sU", ceiling(round(log(max(maxValue(rasterOut))
      , 2)) / 8))
    )
  }
  
#--Return final values--------------------------------------------------------
  return(rasterOut)

}
