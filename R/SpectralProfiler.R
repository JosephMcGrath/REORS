SpectralProfiler <- function(rasterIn, shapeIn = "point", nProfile = 1,
                             returnSpatial = FALSE, plotOut = TRUE,
                             silent = TRUE){
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

#--If creating Spatial* object from scratch, handle it here---------------------
    if (is.character(shapeIn)){
        if (length(shapeIn == 1)){
            if (shapeIn == "point"){
                if (!silent) cat("Click on map to plot points.\n")
                #Doing this in a more-complex manner so that this section
                #behaves the same as lines and polygons (plotting along the way).
                shapeIn <- locator(n = 1)
                shapeIn <- cbind(shapeIn$x, shapeIn$y)
                shapeIn <- SpatialPoints(shapeIn)
                plot(shapeIn[1, ], col = rainbow(nProfile)[1], add = TRUE)
                text(extent(shapeIn)[2],
                     extent(shapeIn)[3],
                     1,
                     col = rainbow(nProfile)[1],
                     pos = 1
                     )
                if (nProfile > 1){
                    for (i in 2:nProfile){
                        temp <- locator(n = 1)
                        temp <- cbind(temp$x, temp$y)
                        temp <- SpatialPoints(temp)
                        text(extent(temp)[2],
                             extent(temp)[3], i,
                             col = rainbow(nProfile)[i],
                             pos = 1
                             )
                        shapeIn <- rbind(shapeIn, temp)
                        plot(shapeIn[i, ],
                             col = rainbow(nProfile)[i],
                             add = TRUE
                             )
                    }
                }
            } else if (shapeIn == "line"){
                if (!silent){
                    cat("Click on map to plot lines.\n")
                }
                shapeIn <- drawLine(col = rainbow(nProfile)[1])
                text(extent(shapeIn)[2],
                     extent(shapeIn)[3],
                     1,
                     col = rainbow(nProfile)[1],
                     pos = 1
                     )
                if (nProfile > 1){
                    for (i in 2:nProfile){
                        temp <- drawLine(col = rainbow(nProfile)[i])
                        text(extent(temp)[2],
                             extent(temp)[3],
                             i,
                             col = rainbow(nProfile)[i],
                             pos = 1
                             )
                        temp <- spChFIDs(temp,
                                         as.character(length(shapeIn) + 1)
                                         )
                        shapeIn <- rbind(shapeIn, temp)
                    }
                }
            } else if (shapeIn == "poly"){
                if (!silent){
                    cat("Click on map to plot lines.\n")
                }
                shapeIn <- drawPoly(col = rainbow(nProfile)[1])
                text(extent(shapeIn)[2],
                     extent(shapeIn)[3],
                     1,
                     col = rainbow(nProfile)[1],
                     pos = 1
                     )
                if (nProfile > 1){
                    for (i in 2:nProfile){
                        temp <- drawPoly(col = rainbow(nProfile)[i])
                        text(extent(temp)[2],
                             extent(temp)[3],
                             i,
                             col = rainbow(nProfile)[i],
                             pos = 1
                             )
                        temp <- spChFIDs(temp,
                                         as.character(length(shapeIn) + 1)
                                         )
                        shapeIn <- rbind(shapeIn, temp)
                    }
                }
            } else {
                stop("Invalid character string for shapeIn.")
            }

            crs(shapeIn) <- crs(rasterIn)
        } else {
            stop("shapeIn may only take one argument.")
        }
    }

#--Extract the data from the raster---------------------------------------------
    if (!silent){
        cat("Extracting values from raster, this may take some time.\n")
    }
    extractType <- class(shapeIn)[[1]]
    if (extractType %in% c("SpatialPolygonsDataFrame", "SpatialPolygons")){
        dataOut <- extract(rasterIn, shapeIn, fun = mean)
    } else if (extractType %in% c("SpatialLinesDataFrame", "SpatialLines")){
        temp <- extract(rasterIn, shapeIn, along = TRUE)
        for (i in 1:length(temp)){
            temp[[i]] <- as.matrix(temp[[i]])
            rownames(temp[[i]]) <- sprintf("L%sP%s", i, 1:nrow(temp[[i]]))
        }
        dataOut <- temp[[1]]
        if (length(temp) > 1){
            for(i in 2:length(temp)){
                dataOut <- rbind(dataOut, temp[[i]])
            }
        }
        if (is.null(colnames(dataOut))){
            colnames(dataOut) <- names(rasterIn)
        }
    } else if (any(c("SpatialPointsDataFrame",
                    "SpatialPoints"
                    ) %in% extractType)){
            dataOut <- extract(rasterIn, shapeIn)
    } else {
        stop("Unable to recognise Spatial* object.")
    }
    if("!silent"){
        cat("Data extracted successfully.\n")
    }
    
#--Output extracted data--------------------------------------------------------
    if (returnSpatial){
        if (!silent){
            cat("Converting to spatial object.\n")
        }
        if (extractType %in% c("SpatialLinesDataFrame", "SpatialLines")){
            rownames(dataOut) <- names(shapeIn)
            dataOut <- data.frame(dataOut)
        }
        if (extractType %in% c("SpatialPolygonsDataFrame",  "SpatialPolygons")){
            dataOut <- SpatialPolygonsDataFrame(shapeIn, dataOut)
        } else if (any(c("SpatialLinesDataFrame",
                         "SpatialLines"
                         ) %in% extractType)){
            dataOut <- list(shapeIn, dataOut)

            #Return pixels along the line as points in this case
            #temp <- extract(rasterIn, AOI, along = TRUE, cellnumbers = TRUE)
            #for(i in 1:length(temp)){
            #    temp[[i]] <- temp[[i]][, 1]
            #    temp[[i]] <- cellFromXY(rasterIn, temp[[i]])
            #}

        } else if (extractType %in% 
                   c("SpatialPointsDataFrame", "SpatialPoints")
                  ){
            dataOut <- SpatialPointsDataFrame(shapeIn, dataOut)
        } else {
            stop("Unable to recognise Spatial* object.")
        }
    }
    return(dataOut)
}
