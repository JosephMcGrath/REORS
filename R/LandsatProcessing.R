LandsatProcessing <- function(filePath, props = NULL,
 fileOut = paste0(c(filePath, "Processed"), collapse = "/"), silent = TRUE){
#Pulls in Landsat bands from single layers and stacks them together. The
# function assumes that the files are the only files in the set folder. Also
# assumes default naming scheme.
#
#To do:
#  Improve file output - should indicate *which* processed file it is.
#
#Args:
#  filePath: The folder containing the bands to be combined 
#   (and no other files). File names should be in their original format.
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
    if (!silent){
        cat("Importing Landsat data.\n")
    }
  
#--Get input images and organise them-------------------------------------------
    if (!silent){
        cat("\tFinding and sorting layers.\n")
    }

    toUse <- list.files(path = filePath,
                        pattern = "TIF",
                        full.names = TRUE
                        )
    #Filtering out XML files - such as those created by QGIS
    #Better long-term solution would be to filter out all non-raster files,
    #rather than just xml files.
    toUse <- grep("xml",
                  toUse,
                  ignore.case = TRUE,
                  value = TRUE,
                  invert = TRUE
                  )

    #Could easily filter out one set if multiple exist in the folder.

    #Converts the text names into integers for proper sorting
    bandOrder <- c()
    for(i in strsplit(toUse, "_B")){
        bandOrder <- append(bandOrder,
                            #Masks out bands without numerical names
                            suppressWarnings(as.numeric(
                                             strsplit(strsplit(i[[2]],
                                                               ".TIF"
                                                               )[[1]],
                                                      "_"
                                                      )[[1]][[1]]
                                            ))
        )
    }

    #This section of code removes duplicates, e.g. the thermal bands of LS7, as
    #this is currently not the desired result this section is commented out.
    #  if(length(bandOrder) != length(unique(bandOrder))){
    #    temp <- rep(NA, length(bandOrder))
    #    for(i in 1:length(bandOrder)){
    #      if(bandOrder[i] %in% temp){
    #        temp[i] <- NA
    #      } else {
    #        temp[i] <- bandOrder[i]
    #      }
    #    }
    #    bandOrder <- temp
    #  }

    toUse[!is.na(bandOrder)] <- toUse[!is.na(bandOrder)][
                                      order(bandOrder[!is.na(bandOrder)])]

#--Check that all layers can be stacked-----------------------------------------
    #Something of a formality as there's a set input format.
    #Prevents the later steps breaking
    rasterTemp <- raster(toUse[1])

    for(i in 2:length(toUse)){
        if(!is.na(bandOrder[i]) &
           compareRaster(rasterTemp, raster(toUse[i]), stopiffalse = FALSE)){
            rasterTemp <- stack(rasterTemp, raster(toUse[i]))
        } else {
            bandOrder[i] <- NA
        }
    }
  
#--Detect the band designations-------------------------------------------------
#Calculates platform number based on both the file names and number of bands.
#Reduces these to the lowest number satellite in the group for simplicity.

#Groups currently are:
  #1 = LS1:3 - four bands (4, 5, 6, 7)
  #4 = LS4:5 - 7 bands
  #7 = LS7 - 8 bands (both IR bands, minus the panchromatic band
  #  due to it's different resolution).
  #8 = LS8 - 10 bands

    #By filename
    lsType1 <- as.numeric(substr(names(rasterTemp)[[1]], 3, 3))

    if(lsType1 >= 1 & lsType1 <= 3){
        lsType1 <- 1
    } else if(lsType1 >= 4 & lsType1 <= 5){
        lsType1 <- 4
    } else if(lsType1 == 7){
        lsType1 <- 7
    } else if(lsType1 == 8){
        lsType1 <- 8
    } else {
        lsType1 <- NA
    }
  
#By number of bands
    numBand <- sum(!is.na(bandOrder))
    if(numBand == 4){
        lsType2 <- 1
    } else if(numBand == 7){
        lsType2 <- 4
    } else if(numBand == 8){
        lsType2 <- 7
    } else if(numBand == 10){
        lsType2 <- 8
    } else {
        lsType2 <- NA
    }
  
    if(any(is.na(c(lsType1, lsType2)))){
        stop("No valid sensor type detected.\n")
    }
    if(lsType1 == lsType2){
        lsType <- lsType1
        if(!silent){
            cat(sprintf("\tSensor group identified as LS %s\n", lsType))
        }
    } else {
        lsType <- lsType2
        warning("Potential error detecting Landsat sensor.\n")
    }
  
    if(lsType == 1){
        bandNames <- c("Green", "Red", "?", "?")
    } else if(lsType == 4){
        bandNames <- c("Blue", "Green", "Red", "Near IR", "Short wave IR  1",
                       "Thermal IR", "Short wave IR 2"
                       )
    } else if(lsType == 7){
        bandNames <- c("Blue", "Green", "Red", "Near IR", "Short wave IR  1",
                       "Thermal IR A", "Thermal IR B", "Short wave IR 2"
                       )
    } else if(lsType == 8){
        bandNames <- c("Costal", "Blue", "Green", "Red", "Near IR",
                       "Short wave IR  1", "Short wave IR  1", "Cirrus",
                       "Panchromatic", "Thermal IR 1", "Thermal IR 2"
                       )
    }
  
  bandNames <- bandNames[!is.na(bandOrder)]
  
  write.table(x = cbind(1:nlayers(rasterTemp),
                        names(rasterTemp), bandNames
                        ),
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
  
#--Mask out all pixels without full set of values-------------------------------
    blocks <- blockSize(rasterTemp)

    rasterOut <- RasterShell(rasterTemp)

    rasterOut <- writeStart(rasterOut,
                            filename = fileOut,
                            format = "GTiff",
                            overwrite = TRUE
                            )

    if(!silent){ 
        cat(sprintf("\tStacking layers.\n\t\tWriting to %s.tif\n", fileOut))
    }
    for(i in 1:blocks$n){
        if(!silent) cat(sprintf("\t\tProcessing block %s of %s\t(%s percent)",
                                i, blocks$n, round(i / blocks$n * 100)
                                ))

        tempValues <- getValues(rasterTemp,
                                row = blocks$row[i],
                                nrow = blocks$nrow[i]
                                )
        if(!silent){
            cat(".")
        }

        tempValues[rowSums(tempValues == 0 | is.na(tempValues)) > 0] <- NA
        if(!silent){
            cat(".")
        }

        rasterOut <- writeValues(x = rasterOut,
                                 v = tempValues,
                                 start = blocks$row[i]
                                 )
        if(!silent){
            cat(".\n")
        }

    }
    rasterOut <- writeStop(rasterOut)

#--Reproject if requested-----------------------------------------------------
#Would ideally put this step before above filtering to potentially reduce the
 #number of cells that need processing, but seems to have a few issues.

    if(class(props)[[1]] %in% c("RasterLayer", "RasterBrick", "RasterStack")){
        if(!silent){
            cat("\tReprojecting image to 'props'.\n")
        }
        rasterOut <- projectRaster(from = rasterOut,
                                    to = props,
                                    filename = fileOut,
                                    format = "GTiff",
                                    overwrite = TRUE,
                                    datatype = sprintf("INT%sU",
                                    ceiling(round(log(max(maxValue(rasterOut)),
                                                          2)) / 8
                                                          ))
                                    )
        #Not the best test for if something is a valid CRS
    } else if(!is.null(props)) {
        if(!silent){
            cat("\tReprojecting image to 'props'.\n")
        }
        rasterOut <- projectRaster(from = rasterOut,
                                   crs = props,
                                   filename = fileOut,
                                   format = "GTiff",
                                   overwrite = TRUE,
                                   datatype = sprintf("INT%sU",
                                   ceiling(round(log(max(maxValue(rasterOut)),
                                                         2)) / 8
                                                         ))
                                   )
    }
  
#--Return final values--------------------------------------------------------
    return(rasterOut)
}
