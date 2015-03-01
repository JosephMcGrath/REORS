TempRasterName <- function(){
  #Wrapper function to generate temporary file names.
  #Separated to allow quicker and more consistent changes.
  return(tempfile(pattern = "REORS - "))
}
