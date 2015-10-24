RasterLoad <- function(dataIn, retForm = "list", fileOut = TempRasterName()){
#Loads data from multiple, potentially different sources into a single object.
#Designed to be inserted into other functions for simplicity.
#
#Args:
#  dataIn: A list of the different files to be conglomerated, formats used:
#   -RasterLayer
#   -RasterBrick
#   -RasterStack
#   -Character vector: containing the names of files to be loaded.
#   -List: containing any of the above, or another list.
#  retForm: Format to return data in, may be:
#   -"list": individual Raster* objects in a list. Mainly intended for rasters
#    that wouldn't be able to stack (different extents ect.). Default option.
#   -"stack": returns all files as a RasterStack.
#   -"brick": returns all files as a RasterBrick. Can take a LONG time.
#  fileOut: The location to save the output, if it is a RasterBrick.
#
#Returns:
#  Either a RasterStack, RasterBrick or list containing the input data.

    library("raster")

    retForm <- tolower(retForm)

#--Convert inputs into a list---------------------------------------------------
    #Defined as a function to unpack nested lists
    RasterLoadRec <- function(rasterIn){
        if(!is.list(rasterIn)){
            rasterIn <- list(rasterIn)
        }
        ret <- list()
        for(i in 1:length(rasterIn)){
            if(class(rasterIn[[i]])[1] == "RasterLayer"){
                ret <- append(ret, rasterIn[[i]])
            }
            if(class(rasterIn[[i]])[1] == "RasterBrick"){
                ret <- append(ret, rasterIn[[i]])
            }
            if(class(rasterIn[[i]])[1] == "RasterStack"){
                ret <- append(ret, rasterIn[[i]])
            }
            if(class(rasterIn[[i]])[1] == "character"){
                for(j in rasterIn[[i]]){
                    if(file.exists(j)){
                        #Should check if the format is compatable?              ToDo
                            #Weed out QGIS xml.
#                        if(extension(j) %in% c("xml"){
#                            next
#                        }
                        temp <- brick(j)
                        if(nlayers(temp) == 1){
                            ret <- append(ret, raster(j))
                        } else {
                            ret <- append(ret, temp)
                        }
                    } else {
                        #Might be better with stop?                             ToDo
                        warning((sprintf("File doesn't exist: %s", j)))
                    }
                }
            }
            if(class(rasterIn[[i]])[1] == "list"){
                ret <- append(ret, RasterLoadRec(rasterIn[[i]]))
            }
        }

        return(ret)
    }

    dataIn <- RasterLoadRec(dataIn)

#--Convert to specified output--------------------------------------------------
    allSame <- TRUE
    if(retForm != "list"){
        for(i in 1:length(dataIn)){
            if(!compareRaster(dataIn[[1]], dataIn[[i]], stopiffalse = FALSE)){
                allSame <- FALSE
            }
        }
    }
    
    #Why not just stop in the above loop?                                       ToDo
        #If keeping this, at least list off all the files that don't share.
    if(!allSame & retForm != "list"){
        stop("Files do not share metadata.")
    }

    #Stacking is very fast, writing a brick is much slower.
    if(allSame & retForm != "list"){
        ret <- stack()
        for(i in dataIn){
            ret <- stack(ret, i)
        }
    } else {
        ret <- dataIn
    }

    if(retForm == "brick"){
        ret <- brick(ret,
                     filename = fileOut,
                     format = "GTiff",
                     overwrite = TRUE
                     )
    }

    return(ret)
}
