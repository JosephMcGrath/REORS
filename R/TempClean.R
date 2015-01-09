TempClean <- function(tDir = tempdir()){
#Cleans the temporary directory of all files belonging to the REORS package.
#As you might imagine, this cannot be undone, take care with this function.
#Searches for file beginning with "REORS" and ending in "tif", shouldn't
# overlap with anything.
#
#Args:
#  The folder to search and clear for temporary files.
#
#Returns:
#  None, deletes files from system.
  
  library("raster")

  file.remove(paste(tDir, list.files(tDir, "REORS.*tif"), sep = "\\"))
  removeTmpFiles(h = 0)
}