"COMPILACIÓN DE DATA GRILLADA DE PRECIPITACIÓNES A DIFERENTES PERIODO DE RETORNO"
#################################################################################
###################*****RARQ********RARQ*******RARQ******########################
#################################################################################
library(raster)
library(ncdf4)
getwd()
setwd("C:/Users/ANTHONYQUIROZ/Documents/BASE_GIS/PRUEBA")
list_raster<-list.files('tif/',pattern = '.tif')
raster_stack<-stack(list_raster)
plot(raster_stack)
spplot(raster_stack)
writeRaster(raster_stack,'RAINFALL_RETURN_PERIOD.nc',format='CDF')
raster_brick<-brick(raster_stack)
spplot(raster_brick)
object.size(raster_stack)
object.size(raster_brick)
writeRaster(raster_brick,'RAINFALL_RETURN_PERIOD_BRICK.nc',format='CDF')
