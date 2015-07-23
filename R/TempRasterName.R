TempRasterName <- function(fileType = "out"){
#Wrapper function to generate temporary file names.
#Separated to allow quicker and more consistent changes.
#
#Args:
#  fileType - the type of file being produced. Current conventions are:
#   "out" - outputs of functions
#   "int" - intermediate files
#
#Returns:
#  A file name as a string.
  
  return(tempfile(pattern = sprintf("REORS -%s- ", fileType)))
}
