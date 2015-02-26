RasterLoad <- function(dataIn, retForm = "list",
 fileOut = REORStemp()){
#Loads data from multiple, potentially different sources into a single object.
#Designed to be inserted into other functions for simplicity.
#
#Args:
#  dataIn: A list of the different files to be conglomerated, formats used:
#   -RasterLayer
#   -RasterBrick
#   -RasterStack
#   -Character vector, containing the names of files to be loaded.
#   -List, containing any of the above, or another list.
#  retForm: Format to return data in, may be:
#   -"stack" for a RasterStack
#   -"brick" for a RasterBrick. May take a LONG time to process
#   -"list"  for a list of objects. Mainly for different extents ect (default)
#  fileOut: The location to save the output, if it is a RasterBrick.
#
#Returns:
#  Either a RasterStack, RasterBrick or list containing the input data.
  
  library("raster")
  
  retForm <- tolower(retForm)
  
#--Convert inputs into a list-------------------------------------------------
#Defined as a function to unpack nested lists
  RasterLoadRec <- function(x){
    if(!is.list(x)) x <- list(x)
    ret <- list()
    for(i in 1:length(x)){
      if(class(x[[i]])[1] == "RasterLayer") ret <- append(ret, x[[i]])
      if(class(x[[i]])[1] == "RasterBrick") ret <- append(ret, x[[i]])
      if(class(x[[i]])[1] == "RasterStack") ret <- append(ret, x[[i]])
      if(class(x[[i]])[1] == "character"){
        for(j in x[[i]]){
          if(file.exists(j)){                                                 #<-- Test if it is a compatible format?
            temp <- brick(j)
            if(nlayers(temp) == 1){
              ret <- append(ret, raster(j))
            } else {
              ret <- append(ret, temp)
            }
          } else warning((sprintf("File doesn't exist: %s", j)))
        }
      }
      if(class(x[[i]])[1] == "list") ret <- append(ret, RasterLoadRec(x[[i]]))
    }
    
    return(ret)
  }
  
  dataIn <- RasterLoadRec(dataIn)
  
#--Convert to specified output------------------------------------------------
  allSame <- TRUE
  if(retForm != "list"){
    for(i in 1:length(dataIn)){
      if(!compareRaster(dataIn[[1]], dataIn[[i]], stopiffalse = FALSE)){
        allSame <- FALSE
      }
    }
  }
  
  if(!allSame & retForm != "list") stop("Files do not share metadata.")
  
  if(allSame & retForm != "list"){
    ret <- stack()
    for(i in dataIn){
      ret <- stack(ret, i)
    }
  } else ret <- dataIn
  
  if(retForm == "brick"){
    ret <- brick(ret, filename = fileOut, format = "GTiff", overwrite = TRUE)
  }

#--Return the conglomerated data----------------------------------------------
  return(ret)
}
