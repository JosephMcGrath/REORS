SpectralProfiler <- function(rasterIn, shapeIn = "point", nProfile = 1,
 returnSpatial = FALSE, plotOut = TRUE, silent = TRUE){
#Calculates the spectral properties of a set of shapes.
#For points, the profile for each point is given.
#For lines, the profile along the line is given.
#For polygons, the average value for pixels under the polygon.
#
#Requires:
#  RasterLoad
#
#To do:
#  -Plot results out
#  -Print numbers onto shapes as they're drawn
#
#Args:
#  rasterIn: Raster* object to extract values from.
#  shapeIn: Either a Spatial* object, or one of the following strings:
#   -"point": To draw points on the plot window.
#   -"line": To draw lines on the plot window.
#   -"poly": To draw polygons on the plot window.
#  nProfile: If creating points or polygons, how many should be created.
#  returnSpatial: If the spectral profiles should be returned as a
#   SpatialDataFrame rather than a matrix.
#  silent: Should information about progress be returned?
#
#Returns:
#  Either a matrix of digital numbers for each point/line/polygon provided or
#   a Spatial* object with the digital numbers attached.
#  For points, values are collected per-pixel.
#  For lines, each pixel along each line is returned in order (as a list of
#   matrices). Due to the many-one relationship of values to lines, spatial
#   results are returned as a list of shapes and values.
#  For polygons, the average value for each polygon is returned.

  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
#--If creating Spatial* object from scratch, handle it here-------------------
  if(is.character(shapeIn)){                                                  #Print numbers onto plotted shapes.
    if(length(shapeIn == 1)){
      if(shapeIn == "point"){
        #Doing this in a more-complex manner so that this section behaves the
         #same as lines and polygons (plotting along the way).
        shapeIn <- locator(n = 1)
        shapeIn <- cbind(shapeIn$x, shapeIn$y)
        shapeIn <- SpatialPoints(shapeIn)
        plot(shapeIn[1, ], col = rainbow(nProfile)[1], add = TRUE)
        if(nProfile > 1) for(i in 2:nProfile){
          temp <- locator(n = 1)
          temp <- cbind(temp$x, temp$y)
          temp <- SpatialPoints(temp)
          shapeIn <- rbind(shapeIn, temp)
          plot(shapeIn[i, ], col = rainbow(nProfile)[i], add = TRUE)
        }
      } else if(shapeIn == "line"){
        shapeIn <- drawLine(col = rainbow(nProfile)[1])
        if(nProfile > 1) for(i in 2:nProfile){
          temp <- drawLine(col = rainbow(nProfile)[i])
          temp <- spChFIDs(temp, as.character(length(shapeIn) + 1))
          shapeIn <- rbind(shapeIn, temp)
        }
      } else if(shapeIn == "poly"){
        shapeIn <- drawPoly(col = rainbow(nProfile)[1])
        if(nProfile > 1) for(i in 2:nProfile){
          temp <- drawPoly(col = rainbow(nProfile)[i])
          temp <- spChFIDs(temp, as.character(length(shapeIn) + 1))
          shapeIn <- rbind(shapeIn, temp)
        }
      } else {
        stop("Invalid character string for shapeIn.")
      }
      
      crs(shapeIn) <- crs(rasterIn)
    } else {
      stop("shapeIn may only take one argument.")
    }
  }
  
#--Extract the data from the raster-------------------------------------------
#Desired format is a matrix with cols for bands and rows for observations
  extractType <- class(shapeIn)[[1]]
  if(length(grep("SpatialPolygons", extractType)) > 0){
    dataOut <- extract(rasterIn, shapeIn, fun = mean)
  } else if(length(grep("SpatialLines", extractType)) > 0){
    temp <- extract(rasterIn, shapeIn, along = TRUE)
    for(i in 1:length(temp)){
      rownames(temp[[i]]) <- sprintf("L%sP%s", i, 1:nrow(temp[[i]]))
    }
    dataOut <- temp[[1]]
    if(length(temp) > 1) for(i in 2:length(temp)){
      dataOut <- rbind(dataOut, temp[[i]])
    }
  } else if(length(grep("SpatialPoints", extractType)) > 0){
    dataOut <- extract(rasterIn, shapeIn)
  } else {
    stop("Unable to recognise Spatial* object.")
  }
  
#--Plot the data if requested-------------------------------------------------
#Currently have issues with non-standard plot windows, e.g. plotRGB
#As data can be easily plotted outside of the function, this isn't really a
 #major issue.
 
#Have an option to standardise all plots to between the same ranges.
  
#  if(plotOut){
#    if(length(grep("SpatialPolygons", extractType)) > 0){
#      plot(x = c(1,ncol(dataOut)), y = c(0, max(dataOut)), type = "n", xlab = "Band #", ylab = "Digital number")
#      for(i in 1:nrow(dataOut)){
#        lines(dataOut[i, ], col = rainbow(nrow(dataOut))[i])
#      }
#    } else if(length(grep("SpatialLines", extractType)) > 0){
#      plot(x = c(1,ncol(dataIn)), y = c(0, max(dataOut)), type = "n", xlab = "Distance (pixels)", ylab = "Digital number")
#                                                                              #Unsure how to plot for multiple line object
#    } else if(length(grep("SpatialPoints", extractType)) > 0){
#      plot(x = c(1,ncol(dataOut)), y = c(0, max(dataOut)), type = "n", xlab = "Band #", ylab = "Digital number")
#      for(i in 1:nrow(dataOut)){
#        lines(dataOut[i, ], col = rainbow(nrow(dataOut))[i])
#      }
#    }
#  }
  
#--Output extracted data------------------------------------------------------
  if(returnSpatial){
    if(length(grep("SpatialLines", extractType)) == 0){
      rownames(dataOut) <- names(shapeIn)
      dataOut <- data.frame(dataOut)
    }
    if(length(grep("SpatialPolygons", extractType)) > 0){
      dataOut <- SpatialPolygonsDataFrame(shapeIn, dataOut)
    } else if(length(grep("SpatialLines", extractType)) > 0){
      dataOut <- list(shapeIn, dataOut)
      
      #Return pixels along the line as points in this case
      #temp <- extract(rasterIn, AOI, along = TRUE, cellnumbers = TRUE)
      #for(i in 1:length(temp)){
      #  temp[[i]] <- temp[[i]][, 1]
      #  temp[[i]] <- cellFromXY(rasterIn, temp[[i]])
      #}
      
    } else if(length(grep("SpatialPoints", extractType)) > 0){
      dataOut <- SpatialPointsDataFrame(shapeIn, dataOut)
    }
  }
  return(dataOut)
}
