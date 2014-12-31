RasterBrowse <- function(retForm = "stack", fileOut = tempfile()){
#Wrapper for RasterLoad, to open a browsing window. 
#

#Args:
#  retForm: Passed to RasterLoad.
#  fileOut: Passed to RasterLoad
#
#Returns:
#  The result of the RasterLoad file, given the set input values.
  library("raster")
  
  return(RasterLoad(choose.files(), retForm = retForm, fileOut = fileOut))
}
