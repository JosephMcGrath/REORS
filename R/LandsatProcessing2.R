filePath <- "C:/GIS Layers/Eye of the Sahara/LE7 203 045 2000 10 23"
fileOut <- "C:/GIS Layers/Eye of the Sahara/LE7 203 045 2000 10 23/Stacked"
crsUse <- NULL
silent <- FALSE
#{

  library("REORS")
  
#--Get input images and organise them-----------------------------------------
  toUse <- list.files(
   path = filePath,
   pattern = "TIF",
   full.names = TRUE
  )
  
  #Converts the text names into integers for proper sorting
  bandOrder <- c()
  for(i in strsplit(toUse, "B")){
    bandOrder <- append(
     bandOrder,
     #Masks out bands without numerical names
     suppressWarnings(as.numeric(strsplit(i[[2]], ".TIF")[[1]]))
    )
  }
  toUse <- toUse[!is.na(bandOrder)][order(bandOrder[!is.na(bandOrder)])]
  
#--Check that all layers can be stacked---------------------------------------
  #Something of a formality as there's a set input format.
  #Prevents the later steps breakig
  rasterTemp <- raster(toUse[1])
  
  for(i in 2:length(toUse)){
    if(compareRaster(rasterTemp, raster(toUse[i]), stopiffalse = FALSE)){
      rasterTemp <- stack(rasterTemp, raster(toUse[i]))
    } else {
      bandOrder[i] <- NA
    }
  }
  
#--Detect the band designations-----------------------------------------------  #<--This is the section that needs checking
#By filename
  lsType1 <- as.numeric(substr(names(rasterTemp)[[1]], 3, 3))
  if(lsType1 > 1 & lsType1 <= 3){
    lsType <- 1
  } else if(lsType1 > 4 & lsType1 <= 7){
    lsType <- 4
  } else if(lsType1 == 8){
    lsType <- 8
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
  
  if(lsType == 1){
    bandNames <- c("Green", "Red", "", "")
  } else if(lsType == 4){
    bandNames <- c("Blue", "Green", "Red", "Near IR", "Short wave IR  1",
     "Thermal IR A", "Thermal IR B", "Short wave IR 2")
  } else if(lsType == 8){
    bandNames <- c("Costal", "Blue", "Green", "Red", "Near IR",
     "Short wave IR  1", "Short wave IR  1", "Cirrus", "Thermal IR 1",
     "Thermal IR 2"
    )
  }
  
  write.table(
   x = cbind(1:length(toUse), names(rasterTemp), bandNames),
   file = sprintf("%s band descriptions.txt", fileOut),
   sep = "\t",
   col.names = FALSE,
   row.names = FALSE,
   quote = FALSE
  )
  
#--Mask out all pixels without full set of values-----------------------------
  blocks <- blockSize(rasterTemp)
  rasterOut <- RasterShell(rasterTemp)
  
  rasterOut <- writeStart(rasterOut, filename = fileOut,
     format = "GTiff", overwrite = TRUE
  )
  
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
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
  if(class(crsUse) == "character"){
    rasterOut <- projectRaster(
     from = rasterOut,
     crs = crsUse,
     filename = fileOut,
     format = "GTiff",
     overwrite = TRUE,
     datatype = sprintf("INT%sU", ceiling(round(log(max(maxValue(rasterOut))
      , 2)) / 8))
    )
  }
  
#--Return final values--------------------------------------------------------
  #return(rasterOut)

#}
