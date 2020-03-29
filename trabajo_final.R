##Trabajo final
#El analisis se hara descargando archivos de temperaturas minimas, medias y maximas como variables
library(rgbif)
library(dismo)
library(XML)
library(maptools)
library(dplyr)

## Pasos previos a la descarga de archivos sin 'hardcode' ####
# URL de la web de datos 'chelsa'
chelsa <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/"

# Links para las temeprtauras de los dos periodos
cuarenta <- "cmip5/2041-2060/"
sesenta <- "cmip5/2061-2080/"
decadas_url <- c(cuarenta, sesenta)

decadas <- c("2041-2060", "2061-2080")

# Siglas que representan los distintos modelos en la descarga
modelos_url <- c("MIROC5", "ACCESS1-0", "FIO-ESM")

# Archivos 2040-2060
fio_model <-  "FIO_SM.tif"
fio_max <-  "FIO_max.tif"
fio_min <- "Fio_min.tif"
acces <- "ACCESS1.tif"
acces_max <- "acces_max.tif"
acces_min <- "Acces_min.tif"
miroc_c5 <- "MIROC_5.tif"
miroc_max <- "miroc_max.tif"
miroc_min <- "miroc_min.tif"

archivos <- c(fio_model, acces, miroc_c5)
archivos_maxima <- c(fio_max, acces_max, miroc_max)
archivos_minima <- c(fio_min, acces_min, miroc_min)
# Descarga de los 6 archivos a utilizar, tres modelos 2041-2060 y otros tres 2061-2080
cont <- 0
cont2 <- 0
for(i in decadas_url){
  cont <- cont+1
  for(j in modelos_url){
    cont2 <- cont2+1
    if(cont2<=3){archivo <- archivos[cont2]
                archivo_max <- archivos_maxima[cont2]
                archivo_min <- archivos_minima[cont2]
                }
    else{archivo <- paste("60",archivos[cont2-3], sep = "")
        archivo_max <- paste("60", archivos_maxima[cont2-3], sep = "")
        archivo_min <- paste("60", archivos_minima[cont2-3], sep = "")
    }
    
    download.file(url = paste(chelsa, i, "temp/CHELSA_tas_mon_",
                              j, "_rcp45_r1i1p1_g025.nc_1_", decadas[cont], "_V1.2.tif", sep=""),
                  destfile = archivo, method = "curl")
    
    download.file(url=paste(chelsa, i, "tmax/CHELSA_tasmax_mon_",
                            j, "_rcp45_r1i1p1_g025.nc_1_", decadas_prec[cont], "_V1.2.tif", sep=""),
                  destfile = archivo_max, method = "curl" )
    
    download.file(url=paste(chelsa, i, "tmin/CHELSA_tasmin_mon_",
                            j, "_rcp45_r1i1p1_g025.nc_1_", decadas_prec[cont], "_V1.2.tif", sep=""),
                  destfile = archivo_min, method = "curl" )
  }}
cont <- 0  


# Archivos 2060-2080
fio_model2 <-  "60FIO_SM.tif"
fio_max_2 <-  "60FIO_max.tif"
fio_min_2 <- "60Fio_min.tif"

acces2 <- "60ACCESS1.tif"
acces_max_2 <- "60acces_max.tif"
acces_min_2 <- "60Acces_min.tif"

miroc_max_2 <- "60miroc_max.tif"
miroc_min_2 <- "60miroc_min.tif"
miroc_c5_2 <- "60MIROC_5.tif"


# Especia elegida
reno <- gbif("Rangifer", "tarandus platyrhynchus", download=TRUE)
head(reno)
str(reno)
getwd()
write.csv(reno, "reno_ocurrencia.csv")
plot(reno$lat ~ reno$lon, pch=16, cex=0.6, col="red")

## Predicciones para Suecia entre 2040 y 2060 ####
# Archivos raster manipulables
temp_acces <- raster(acces)
temp_fio <- raster(fio_model)
temp_miroc <- raster(miroc_c5)

## Predicciones para Suecia entre 2060 y 2080 ####
# Archivos raster manipulables
temp_acces2 <- raster(acces2)
temp_fio2 <- raster(fio_model2)
temp_miroc2 <- raster(miroc_c5_2)
e <- extent(10, 24, 76, 81)

# Se divide entre 10 segun indicaciones de "Chelsa" para obtener los grados centigrados
pred_acces <- crop(temp_acces, e)/10
pred_fio <- crop(temp_fio, e)/10
pred_miroc <- crop(temp_miroc, e)/10

# Predicciones para el periodo entre el 60 y el 80
pred_acces2 <- crop(temp_acces2, e)/10
pred_fio2 <- crop(temp_fio2, e)/10
pred_miroc2 <- crop(temp_miroc2, e)/10

summary(reno[,c("lat","lon")])


data(wrld_simpl) #creamos el mapa de españa
plot(pred_acces)

points(reno$lat~reno$lon, pch=20, cex=0.6, col="red") #puntos del lince
