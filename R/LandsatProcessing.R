LandsatProcessing <- function(filePath, LsType = NA,
 crsUse = NULL, silent = TRUE){
#A function to pull landsat data into an easily usable format.
#Still a bit of a work in progress, seems to mess with the automatic min/max.
#Assumes the data has been unzipped and all the .tif files in the folder are
# relevant. Currently (2014) works for default Landsat download format.
#
#To do:                                                                       <--Add features & streamline
#-Change the order the bands are taken in - might have to write exceptions for
# each case though. Maybe manually add a 0 to front of the <10 bands.
#-FIX MASKING so that it doesn't break in ERDAS :/
# maybe just apply over a writeStart loop? Add in for other parts too.
#
#Note: Takes a while, during testing taken 30 minutes +
#
#Args:
#  filePath: The folder containing the bands to be combined
#   Can go several folders deep, but will be return odd file names.
#  LsType: The number of the landsat sattelite being processed, main
#   importance is fixing the band order of LS8 due to autosorting issues.
#  crsUse: Optional input for coordinate reference system to project the
#   output into, if omitted, projection is not changed..
#  silent: should the function work without progress reports?
#
#Returns:
#  None, files are written to disk in working directory, file names will be
#   the same as folder containing the files.

#--Define where to look for files---------------------------------------------
  library("raster")
  #Assigns a number to each output, to make it clearer what came from what
  outputNo <- 1

#--Pull in all of the .tif files----------------------------------------------
  toUse <- list.files(
   path = sprintf("./%s", filePath),
   pattern = "TIF",
   full.names = TRUE
  )
  
  rastersIn <- list()
  for(i in toUse){
    rastersIn <- append(
     rastersIn,
     raster(i)
    )
  }
  
  if(!silent) cat("Files found and loaded\n")
  
#--Exclude all of unequal properties------------------------------------------
  toUse <- lapply(lapply(rastersIn, res), mean)
  toUse <- toUse == toUse[[1]]
  
  rastersIn <- rastersIn[toUse]
  
  #Placeholder until I work out how to get it to automatically sort the bands
   #though it's enough of a corner case that it may just stay this way.
  if(LsType == 8){
    rastersIn <- rastersIn[c(1,4,5,6,7,8,9,10,2,3,11)]
  }
  
  if(!silent) cat("Non-standard resolution layers rejected\n")
  
#--Create a brick of all the files--------------------------------------------
  if(LsType == 7){
    bandNames <- c("Blue", "Green", "Red", "Near IR", "Short wave IR  1",
     "Thermal IR A", "Thermal IR B", "Short wave IR 2")
  } else if(LsType == 8){
    bandNames <- c("Costal", "Blue", "Green", "Red", "Near IR",
     "Short wave IR  1", "Short wave IR  1", "Cirrus", "Thermal IR 1",
     "Thermal IR 2", "Quality control"
    )
  }
  
  rastersIn <- stack(x = rastersIn)
  write.table(
   x = cbind(1:length(names(rastersIn)), names(rastersIn), bandNames),
   file = sprintf("%s %s bands.txt", filePath, outputNo),
   sep = "\t",
   col.names = FALSE,
   row.names = FALSE,
   quote = FALSE
  )
  
  outputNo <- outputNo + 1

  rastersIn <- brick(
   rastersIn,
   filename = sprintf("%s %s stacked", filePath, outputNo),
   format = "GTiff",
   overwrite = TRUE,
   datatype = sprintf("INT%sU", ceiling(round(log(max(maxValue(rastersIn))
    , 2)) / 8))
  )
  
  outputNo <- outputNo + 1
  if(!silent) cat("Layers stacked\n")
  
#--Re project the raster if requested-----------------------------------------
  if(class(crsUse) == "character"){
    rastersIn <- projectRaster(
     from = rastersIn,
     crs = crsUse,
     filename = sprintf("%s %s reprojected", filePath, outputNo),
     format = "GTiff",
     overwrite = TRUE,
     datatype = sprintf("INT%sU", ceiling(round(log(max(maxValue(rastersIn))
      , 2)) / 8))
    )
    
    outputNo <- outputNo + 1
    if(!silent) cat("Reprojected\n")
  }
    
#--Mask out all cells without values------------------------------------------
  rastersIn <- mask(                                                          #<--Mask all layers for all others, not just themselves
   rastersIn,
   rastersIn,
   maskvalue = 0,
   filename = sprintf("%s %s masked", filePath, outputNo),
   format = "GTiff",
   overwrite = TRUE,
   datatype = sprintf("INT%sU", ceiling(round(log(max(maxValue(rastersIn))
    , 2)) / 8))
  )
  
  outputNo <- outputNo + 1
  if(!silent) cat("Layers masked\n")
  
  if(!silent) cat("Operation complete\n")
}
