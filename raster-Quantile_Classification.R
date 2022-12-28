library("raster")

setwd("//kerapremc01/Geo_Spatial/2.0.Projects/Fiber/FOLDER MANAGEMENT PROGRESS/Development Project/RnD Growth Modeling using LiDAR/CSI/Raster/Spatial Parameter/Auto_Cropping_Raster")
list.files(pattern=".tif$")


# list the value that need a Quantile Classification
Quantile_Cls <- function(Parameter){
  parameter <- raster(Parameter)
  QR <- quantile(parameter, probs = c(0.2, 0.4,0.6,0.8), type=7,names = FALSE)
  QR2 <- c(-Inf, QR[[1]], 1,
           QR[[1]], QR[[2]], 2,
           QR[[2]], QR[[3]], 3,
           QR[[3]], QR[[4]], 4,
           QR[[4]], Inf, 5)
  Reclass <- matrix(QR2,ncol = 3, byrow = TRUE)
  parameter <- reclassify(parameter,Reclass)
  name <- read.table(text=Parameter, sep=".", as.is = T)$V1
  writeRaster(parameter,paste(name,"_Cls.tif",sep=""), overwrite=TRUE)
  plot(parameter[[1]], col=brewer.pal(n = 5, name = "Spectral"))
}
Quantile_Cls("Elevasi.TEEE065.tif")
