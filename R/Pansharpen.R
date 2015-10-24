Pansharpen <- function(multiIn, panIn, bands, meth = "brovey",
                       fileOut = TempRasterName(), silent = TRUE){
#Applies pan-sharpening to a multispectral image to increase the apparent
# resolution of the input. Care should be taken that the panchromatic band
# is relevant to the multispectral bands being sharpened.
#
#To do:
#  Alternative methods, e.g. HSI transformation.
#
#Sources:
#  The equation for brovey pan-sharpening was taken from:
#  Scarp G. 2014. Spectral and spatial quality analysis of pan-sharpening
#  algorithms: A case study in Istanbul. European Journal of Remote
#  Sensing 47. 19-28.
#
#Requires:
#  RasterLoad, RasterShell
#
#Args:
#  multiIn: The multispectral image to be sharpened.
#  panIn: The higher resolution panchromatic image, the properties of this
#   determines properties of the output.
#  bands: The bands of the multispectral image to use. Will be re-ordered
#         into the given sequence.
#  meth: The method to use for pansharpening:
#   - "brovey": normalises the input based on total values for each pixel
#               see references for details.
#  fileOut: The name of the file to write out, defaults to a temporary file.
#  silent: Should the function work without progress reports?
#
#Returns:
#  A RasterBrick of the pansharpened image. Note this image is unsuitable for
#   most forms of further processing as values have been altered away from
#   true reflectances.

    library("raster")
    library("REORS")

#--Prepare multispectral imagery------------------------------------------------
    if(!silent){
        cat("Re-sampling multispectral input to panchromatic input.\n")
    }

    if(nlayers(panIn) > 1){
        stop("Panchromatic input can only have one band.\n")
    }

    multiIn <- RasterLoad(multiIn, retForm = "stack")
    if(any(!bands %in% 1:nlayers(multiIn))){
        stop("Multispectral input doesn't contain those bands.\n")
    }
    rasterIn <- stack()
    for(i in 1:length(bands)){
        rasterIn <- stack(rasterIn, raster(multiIn, bands[i]))
    }

    rasterIn <- projectRaster(from = rasterIn, to = panIn)

    meth <- tolower(meth)

#--Set-up for processing--------------------------------------------------------
    blocks <- blockSize(rasterIn, n = length(bands) + 1)
    rasterOut <- RasterShell(rasterIn)

    rasterOut <- writeStart(rasterOut,
                            filename = fileOut,
                            format = "GTiff",
                            overwrite = TRUE
                            )

    if(!silent){
        cat(sprintf("Pan-sharpening image:\n\tWriting to %s.tif\n",
                    fileOut
                    ))
  
#--Process each block in turn---------------------------------------------------
    for(i in 1:blocks$n){
        if(!silent){
            cat(sprintf("\tProcessing block %s of %s\t(%s percent)",
                        i,
                        blocks$n,
                        round(i / blocks$n * 100)
                        ))
        }

        tempValues <- getValues(rasterIn,
                                row = blocks$row[i],
                                nrow = blocks$nrow[i]
                                )

        tempPan <- as.numeric(getValues(panIn,
                              row = blocks$row[i],
                              nrow = blocks$nrow[i]
                              ))
        if(!silent){
            cat(".")
        }

        if(meth == "brovey"){
            tempValues2 <- (tempValues * tempPan) / rowSums(tempValues)
        } else {
            stop("No valid method defined.\n")
        }
        if(!silent){
            cat(".")
        }

        rasterOut <- writeValues(x = rasterOut,
                                 v = tempValues2,
                                 start = blocks$row[i]
                                 )
        if(!silent){
            cat(".\n")
        }
    }

    rasterOut <- writeStop(rasterOut)

#--Post-processing goes here--------------------------------------------------
    return(rasterOut)
}
