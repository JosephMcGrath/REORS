RasterBrowse <- function(retForm = "stack",
 fileOut = REORStemp()){
#Wrapper for RasterLoad, to open a browsing window. 
#
#Requires: RasterLoad
#
#Args:
#  retForm: Passed to RasterLoad.
#  fileOut: Passed to RasterLoad
#
#Returns:
#  The result of the RasterLoad file, given the set input values.
  
  library("raster")
  library("REORS")
  
  return(RasterLoad(choose.files(), retForm = retForm, fileOut = fileOut))
}
