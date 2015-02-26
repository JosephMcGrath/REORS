REORStemp <- function(){
#Wrapper function to generate temporary file names.
#Separated to allow easier changing and to keep things consistent.
  return(tempfile(pattern = "REORS - "))
}
