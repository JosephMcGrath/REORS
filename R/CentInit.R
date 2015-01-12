CentInit <- function(rasterIn, nCentres, meth = "rand", silent = TRUE){
#Several methods of generating initial centres for a centroid-based clustering
# algorithm. A few are also implemented as part of the algorithms, but the
# more time-intenisve ones are isolated here.
#
#Args:
#  rasterIn: The Raster* object to use, passed through RasterLoad.
#  nCentres: How many clusters should the data be split into?
#  meth: The method used to initialise centres, one of:
#   -"rand" : A simple random distribution between the minimum and maximum
#    values per layer.
#   -"lin"  : Distributed linearly between the minimum and maximum values
#    per layer.
#   -"prin" : (NYI) Linear distribution along the first prinicpal component.
#   -"dist" : Distributed randomly, but with probability based on data in.
#  silent: Should the function work without progress reports?
#
#Returns:
#  A matrix of cluster centres with columns as centres and rows as layers.

  library("raster")
  library("REORS")
  
  #Check & clean up input
  if(!is.character(meth)) stop("\"meth\" must be a character vector.\n")
  meth <- tolower(meth)
  if(meth == "linear") meth <- "lin"
  if(meth == "principal") meth <- "prin"
  if(meth == "principal components") meth <- "prin"
  if(meth == "random") meth <- "rand"
  if(meth == "distributed") meth <- "dist"
  if(meth == "distributed random") meth <- "dist"
  
  rasterIn <- RasterLoad(rasterIn, "stack")
  #rasterIn <- setMaxMin(rasterIn)
  
  ret <- matrix(ncol = nCentres, nrow = nlayers(rasterIn))
  colnames(ret) <- sprintf("Clust %s", 1:nCentres)
  rownames(ret) <- names(rasterIn)
  
  if(meth == "rand"){
    for(i in 1:nrow(ret)){
      ret[i, ] <- runif(nCentres, min = minValue(rasterIn)[i],
       max = maxValue(rasterIn)[i])
    }
  }
  if(meth == "lin"){
    for(i in 1:nrow(ret)){
      ret[i, ] <- seq(from = minValue(rasterIn)[i],
       to = maxValue(rasterIn)[i], length.out = nCentres)
    }
  }
  if(meth == "prin"){
    stop("NTI")
  }
  if(meth == "dist"){
    if(!silent) cat("Calculating density of input.\n")
    den <- density(rasterIn, plot = FALSE)
    if(!silent) cat("Calculating centres matrix.\n")
    for(i in 1:nrow(ret)){
      ret[i, ] <- sample(den[[i]]$x, nCentres,
       prob = den[[i]]$y, replace = TRUE) + runif(1, 0, den[[i]]$bw)
    }
  }
  
  return(ret)
}
