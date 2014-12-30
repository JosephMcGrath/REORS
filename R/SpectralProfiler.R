SpectralProfiler <- function(rasterIn, nProfile = 1, type = "points",
 plotL = 1, stand = FALSE, silent = TRUE, shapeIn = NULL, csvOut = NULL){
#Calculates the spectral profile of a set zone.
#For lines and polygoins, calculates the mean values.
#
#To-do notes:
#-All options should output a spatial* object, currently points only gives
# coordinates.
#-Proper key for profile plotting.
#
#Requires: RasterLoad
#
#Args:
#  rasterIn: Raster* object to extract values from.
#  nProfile: Number of profiles to take (ignored for "lines" & "shape")
#  type: What type of input should be used?
#   -points: Profiles for each point.
#   -lines: Profiles of changing spectral values along a line.
#   -poly : Profiles of mean values over polygons.
#   -shape: Profiles using a pre-defined shapefile, not yet implemented.
#  plotL: Which layer should be used to plot when selecting areas?
#  stand: Should the data be standardised before plotting?
#  silent: Should progress be printed to the console?
#  shapeIn: If type is "shape", this is where the shapefile is given.
#  csvOut: A string of the file name to write the profile to. If omitted, the
#   file is not written.
#
#Returns:
#  A list of two items:
#   -Matrix of DN values
#   -Coordinates/shapefile that was used to create them.

  library("raster")
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  specProf <- matrix(nrow = nProfile, ncol = nlayers(rasterIn))
  
  #Common typos
  if(type == "point") type <- "points"
  if(type == "polygon") type <- "poly"
  if(type == "polygons") type <- "poly"
  if(type == "polys") type <- "poly"
  if(type == "line") type <- "lines"
  
#--Calculation using shape-files----------------------------------------------
  if(type == "shape"){
  
    if(class(shapeIn)[1] == "SpatialLines" |
     class(shapeIn)[1] == "SpatialLinesDataFrame"){
      specProf <- t(extract(rasterIn, shapeIn, along = TRUE)[[1]])
      type <- "line" #For plotting later
      #Also limit to one line
    } else if(class(shapeIn) == "list"){
      for(i in 1:length(shapeIn)){
        specProf <- extract(rasterIn, shapeIn[[i]], "mean")
      }
    } else if(class(shapeIn) == "matrix"){
      specProf <- rasterIn[cellFromXY(rasterIn, shapeIn)]
    } else {
      specProf <- extract(rasterIn, shapeIn, "mean")
    }
    
    AOI <- shapeIn
  }
  
#--Calculation using polygons-------------------------------------------------
  if(type == "poly"){
    AOI <- list()
    plot(rasterIn, plotL)
    
    if(!silent) cat("Click on the plot window to mark on areas to test\n")
    
    for(i in 1:nProfile){
      AOI <- append(AOI, drawPoly(col = rainbow(nProfile)[i]))
    }
    
    for(i in 1:length(AOI)){
      specProf[i, ] <- extract(rasterIn, AOI[[i]], fun = mean)
    }
  }
  
#--Calculation using points---------------------------------------------------
  if(type == "points"){
    plot(rasterIn, plotL)
    AOI <- locator(n = nProfile)
    AOI <- cbind(AOI$x, AOI$y)
    specProf <- rasterIn[cellFromXY(rasterIn, AOI)]
  }
  
#--Calculation using lines----------------------------------------------------
  if(type == "line"){
    plot(rasterIn, 1)
    
    if(!silent) cat("Click on the plot window to mark on areas to test\n")
    
    AOI <- drawLine()
    
    specProf <- t(extract(rasterIn, AOI, along = TRUE)[[1]])
  }
  
#--Plot the profiles and return matrix----------------------------------------
  if(stand){
    specProf <- apply(
     X = specProf,
     MARGIN = 2,
     FUN = function(x) (x - min(x)) / (max(x) - min(x))
    )
  }
  
  if(!silent) cat("Plotting stats\n")
  
  if(type == "line"){
    xLabel <- "Distance (pixels)"
    colnames(specProf) <-1:ncol(specProf)
  } else {
    xLabel <- "Band #"
    colnames(specProf) <- sprintf("Band %s", 1:ncol(specProf))
  }
  if(stand){
    yLabel <- "Standardised DN"
  } else {
    yLabel <- "DN"
  }
  
  plot(c(1, ncol(specProf)),
   c(min(specProf, na.rm = TRUE), max(specProf, na.rm = TRUE)),
   type = "n", xlab = xLabel, ylab = yLabel
  )
  
  for(i in 1:nrow(specProf)){
    if(type != "line"){
      points(1:ncol(specProf), specProf[i, ], 
       col = rainbow(nrow(specProf))[i], pch = 4
      )
    }
    lines(1:ncol(specProf), specProf[i, ], col = rainbow(nrow(specProf))[i])
  }
  
#--Write to file
  if(!is.null(csvOut)){
    write.table(specProf, file = sprintf("%s.csv", csvOut),
     sep = ",", row.names = FALSE
    )
  }
  
  return(list("Profile" = specProf, "Area used"= AOI))
}
