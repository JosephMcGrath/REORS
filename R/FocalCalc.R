FocalCalc <- function(rasterIn, sumFun, kernelSize, kernelShape = "circle",
 na.rm = FALSE, fileOut = tempfile(pattern = "REORS - "), silent = TRUE){
#Takes the area surrounding a given Raster* object (one or several layers) and
# summarises them based on a used-defined, or pre-set function.
#
#This function adds support for multi-layered Raster* objects, which at the
# time of writing are not supported by the raster package's "focal" function.
#
#Requires: RasterLoad, RasterShell
#
#Hypsometric integral source: 
# Pike,R.J., Wilson,S.E. (1971). 
# Elevation-relief ratio, hypsometric integral, and geomorphic area-altitude
# analysis. Geological Society of America Bulletin, 82, 1079-1084
#
#Topographic openness source:
# Yokoyama R., Shirasawa M., Pike R.J. (2002)
# Visualizing Topography by Openness: A New Application of Image Processing to
# Digital Elevation Models. Photogrammetric Engineering & Remote Sensing.
# 68, 257-265
#
#Args:
#  rasterIn: The raster file to use in calculation, passed through RasterLoad
#  sumFum: The function used to summarise the focal values. Should return a
#   single value (if multiple are returned, will use the first). Several
#   presets are available using strings:
#   -"hyps": The hypsometric integral
#   -"range": The difference between the maximum and minimum values in the
#    kernel.
#   -"wmean": Weighted mean, for when kernel values aren't equal.
#   -"amin": The difference between the centre of the kernel and the minimum
#    elevation in the kernel.
#   -"bmax": The difference between the centre of the kernel and the maximum
#    elevation in the kernel.
#   -"topoOpennessP": The positive topographic openness. Assumes the input is
#    a DEM and that horizontal and vertical units are the same.
#   -"topoOpennessN": The negative topographic openness. Assumes the input is
#    a DEM and that horizontal and vertical units are the same.
#  kernelSize: The size of the kernel to be used. May be either a single
#   number (for a square matrix), a pair of numbers (for uneven height &
#   width). Alternatively a pre-set matrix can be given (in which case
#   kernelShape will be ignored), NA values can be given for pixels to be
#   ignored. Note: kernel MUST have odd dimensions, can use NA values to
#   simulate even sides.
#  kernelShape: The weighting/shape to give the kernel. The values in the
#   kernel are multiplied by these - so take this into account when using
#   non NA/1 values. The following options are available:
#   -"square": All values in the kernel are set to 1
#   -"circle": A circle of 1 values in the centre of the matrix, surrounded
#    by NA values. With smaller kernels, the shape can be distorted.
#   -"gaussian: Decreasing weighting outwards, based on Gaussian distribution.
#    Sigma value for distribution is set to 1.
#  na.rm: Ignores NA values in the Raster* input.
#  fileOut: Name to be used in output, defaults to temporary file.
#  silent: Should details of the classification be output as it works?
#
#Returns:
#  A Raster* object with the same number of layers as input containing the
#   calculated values.

  library("raster")
  library("REORS")
  
  rasterIn <- RasterLoad(rasterIn, retForm = "stack")
  
  if(kernelSize %% 2 != 1){
    stop("Kernel must have odd number of cells per side.\n")
  }
  
#--Set up the weighting matrix-------------------------------------------------
  #Generate the initial values
  noMod <- FALSE
  if(class(kernelSize) == "matrix"){
    kernelUse <- kernelSize
    noMod <- TRUE
  } else if(class(kernelSize) == "numeric"){
    if(length(kernelSize) == 2){
      kernelUse <- matrix(1, ncol = kernelSize[1], nrow = kernelSize[2])
    } else if(length(kernelSize) == 1){
      kernelUse <- matrix(1, ncol = kernelSize, nrow = kernelSize)
    } else {
      stop("Invalid kernel definition.")
    }
  } else {
    stop("Invalid kernel definition.")
  }
  
  #Apply shape to the weightings
  if(!noMod){
    if(kernelShape == "circle"){
      mid <- c(ceiling(ncol(kernelUse) / 2), ceiling(nrow(kernelUse) / 2))
      for(i in 1:ncol(kernelUse)){
        for(j in 1:nrow(kernelUse)){
          if(sqrt((i - mid[1]) ^ 2 + (j - mid[2]) ^ 2) > (mean(mid) - 1)){
            kernelUse[i, j] <- NA
          }
        }
      }
    } else if(kernelShape == "gaussian"){
      mid <- c(ceiling(ncol(kernelUse) / 2), ceiling(nrow(kernelUse) / 2))
      for(i in 1:ncol(kernelUse)){
        for(j in 1:nrow(kernelUse)){
          kernelUse[i, j] <- 1 / (2 * pi * (1 ^ 2)) * #1 ^ 2 here is sigma
           exp(-1 * (((i - mid[1]) / (0.2 * ncol(kernelUse))) ^ 2 +
           ((j - mid[2]) / (0.2 * nrow(kernelUse))) ^ 2) / 2 *
           1 ^ 2) #The 1 in 1 ^ 2 here is sigma
        }
      }
    }
  }
    
  if(!silent){
    cat("Kernel used:\n")
    print(kernelUse)
  }
  
  #Finally "flatten" out the kernel
  ngbSize <- c(nrow(kernelUse), ncol(kernelUse))
  kernelUse <- c(kernelUse)
  
#--Predefined functions-------------------------------------------------------
  if(sumFun == "topoOpennessN" | sumFun == "topoOpennessP"){
      kernelDist <- matrix(NA, ngbSize[1], ngbSize[2])
      cent <- ceiling(ngbSize / 2)
      for(i in 1:nrow(kernelDist)){
        for(j in 1:nrow(kernelDist)){
          kernelDist[i, j] <- sqrt(((i - cent[1]) * res(rasterIn)[1]) ^ 2 +
           ((j - cent[2]) * res(rasterIn)[2]) ^ 2)
        }
      }
      kernelDist[cent[1], cent[2]] <- NA
      
      mid <- ceiling(length(kernelDist) / 2)
      diam <- floor(nrow(kernelDist) / 2)
      pulls <- list(
       seq(1, mid, nrow(kernelDist) + 1),
       seq(mid, nrow(kernelDist), -1 * nrow(kernelDist) + 1),
       seq(mid, length(kernelDist), nrow(kernelDist) + 1),
       seq(mid, length(kernelDist) - nrow(kernelDist) + 1, nrow(kernelDist) - 1),
       seq(ceiling(nrow(kernelDist) / 2), mid, nrow(kernelDist)),
       seq(mid, length(kernelDist) - diam, nrow(kernelDist)),
       seq(mid - diam, mid),
       seq(mid, mid + diam)
      )
      
      kernelDist <- kernelDist[!is.na(kernelUse)]
      for(i in 1:length(pulls)){
        pulls[[i]] <- pulls[[i]][!(pulls[[i]] %in% which(is.na(kernelUse)))]
      }
    }
  
  if(class(sumFun) == "character"){
    if(sumFun == "hyps"){
      sumFun <- function(x) return((mean(x) - min(x)) / (max(x) - min(x)))
    } else if(sumFun == "range"){
      sumFun <- function(x) return(max(x) - min(x))
    } else if(sumFun == "wmean"){
      sumFun <- function(x) return(sum(x) / sum(kernelUse))
    } else if(sumFun == "amin"){
      sumFun <- function(x) return(x[ceiling(length(x) / 2)] - min(x))
    } else if(sumFun == "bmax"){
      sumFun <- function(x) return(max(x) - x[ceiling(length(x) / 2)])
    } else if(sumFun == "topoOpennessP"){
      sumFun <- function(x){
        x <- atan((x - x[mid]) / kernelDist) / (2 * pi) * 360
        return(mean(
         max(x[pulls[[1]]], na.rm = TRUE), max(x[pulls[[2]]], na.rm = TRUE),
         max(x[pulls[[3]]], na.rm = TRUE), max(x[pulls[[4]]], na.rm = TRUE),
         max(x[pulls[[5]]], na.rm = TRUE), max(x[pulls[[6]]], na.rm = TRUE),
         max(x[pulls[[7]]], na.rm = TRUE), max(x[pulls[[8]]], na.rm = TRUE)
        ))
      }
    } else if(sumFun == "topoOpennessN"){
      sumFun <- function(x){
        x <- atan((x - x[mid]) / kernelDist) / (2 * pi) * 360
        return(mean(
         min(x[pulls[[1]]], na.rm = TRUE), min(x[pulls[[2]]], na.rm = TRUE),
         min(x[pulls[[3]]], na.rm = TRUE), min(x[pulls[[4]]], na.rm = TRUE),
         min(x[pulls[[5]]], na.rm = TRUE), min(x[pulls[[6]]], na.rm = TRUE),
         min(x[pulls[[7]]], na.rm = TRUE), min(x[pulls[[8]]], na.rm = TRUE)
        ))
      }
    } else stop("Invalid summary function defined.\n")
  }
  
#--Apply the filter to the data-----------------------------------------------
  #Using conservatively small blocks to keep things manageable.
  #Results in more read/write cycles.
  #n = kernel * nlayers? For Raster*'s with many layers.
  blocks <- blockSize(rasterIn, n = length(kernelUse))
  rasterOut <- RasterShell(rasterIn)
  
  rasterOut <- writeStart(rasterOut, filename = fileOut, format = "GTiff",
   overwrite = TRUE)
  
  if(!silent) cat(sprintf("Applying focal operation:\nWriting to %s\n",
   fileOut))
  
  for(i in 1:blocks$n){
    if(!silent) cat(sprintf("\tProcessing block %s of %s\t(%s percent)\n",
     i, blocks$n, round(i / blocks$n * 100)))
     
    tempValues <- getValuesFocal(
     rasterIn,
     row = blocks$row[i],
     nrow = blocks$nrow[i],
     ngb = ngbSize,
     padValue = NA
    )
    
    if(nlayers(rasterIn) == 1){
      writeV <- rep(NA, nrow(tempValues))
      tempValues <- t(t(tempValues) * kernelUse)
      for(j in 1:nrow(tempValues)){ #Using "j" for easy conversion below
        if(!any(is.na(tempValues[j, !is.na(kernelUse)])) | na.rm){
          writeV[j] <- sumFun(tempValues[j, ][!is.na(tempValues[j, ])])[[1]]
        }
      }
    } else {
      writeV <- matrix(
       NA,
       nrow = nrow(tempValues[[1]]),
       ncol = length(tempValues)
      )
      for(k in 1:length(tempValues)){
        tempValues[[k]] <- t(t(tempValues[[k]]) * kernelUse)
        for(j in 1:nrow(tempValues[[k]])){
          if(!any(is.na(tempValues[[k]][j, !is.na(kernelUse)])) |
           na.rm){
            writeV[j, k] <- sumFun(tempValues[[k]][j, ][!is.na(
             tempValues[[k]][j, ])])[[1]]
          }
        }
      }
    }
    
    rasterOut <- writeValues(
     x = rasterOut,
     v = writeV,
     start = blocks$row[i]
    )
    
  }
  
#--Finish writing and return the final values---------------------------------  
  rasterOut <- writeStop(rasterOut)

  return(rasterOut)
}
