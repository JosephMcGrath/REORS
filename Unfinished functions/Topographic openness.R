#Topographic openness calculation.
rasterIn <- raster()
  
  distMat <- WMat(5)
  
  cent <- c(ceiling(nrow(distMat) / 2), ceiling(ncol(distMat) / 2))
  
  for(i in 1:nrow(distMat)){
    for(j in 1:ncol(distMat)){
      distMat[i, j] <- sqrt( (cent[1] - i) ^ 2 + (cent[2] - j) ^ 2)
    }
  }
  
  temp <- focal(
   x = rasterIn,
   w = WMat(5),
   filename = tempfile(),
   format = "GTiff",
   fun = function(x){
     mean(c(
#Rook movement
      max(distMat[cent[1], 1:cent[2] - 1], na.rm = TRUE),
      max(distMat[cent[1], (cent[2] + 1):ncol(distMat)], na.rm = TRUE),
      max(distMat[1:cent[1] - 1, cent[2]], na.rm = TRUE),
      max(distMat[(cent[1] + 1):nrow(distMat), cent[2]], na.rm = TRUE),
  
#Queen movement
      max(distMat[seq(1, nrow(distMat) * cent[2] - cent[1], ncol(distMat) + 1)], na.rm = TRUE),
      max(distMat[seq(1 + cent[1] + nrow(distMat) * cent[2], ncell(distMat), 1 * ncol(distMat) + 1)], na.rm = TRUE),
      max(distMat[seq(nrow(distMat), (ncol(distMat) - 1) * cent[2], ncol(distMat) - 1)], na.rm = TRUE),
      max(distMat[seq(cent[1] * ncol(distMat) + 1, ncell(distMat), ncol(distMat) - 1)], na.rm = TRUE)
     ))
   }
  )
