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


# Vemos la lat y la lon para hacer el clip y ver el reno
summary(reno[,c("lat","lon")])


# Se cargan todos los modelos mediante la funcion raster y en la zona vista del habitat del reno
e <- extent(10, 24, 76, 81)

fio_model <- crop(raster(fio_model), e)/10
acces <- crop(raster(acces), e)/10
miroc_c5 <- crop(raster(miroc_c5), e)/10
fio_max <- crop(raster(fio_max), e)/10
fio_min <- crop(raster(fio_min), e)/10
acces_max <- crop(raster(acces_max), e)/10
acces_min <- crop(raster(acces_min), e)/10
miroc_max <- crop(raster(miroc_max), e)/10
miroc_min <- crop(raster(miroc_min), e)/10
fio_model2 <-  crop(raster(fio_model2), e)/10
fio_max_2 <-  crop(raster(fio_max_2), e)/10
fio_min_2 <- crop(raster(fio_min_2), e)/10
acces2 <- crop(raster(acces2), e)/10
acces_max_2 <- crop(raster(acces_max_2), e)/10
acces_min_2 <- crop(raster(acces_min_2), e)/10
miroc_max_2 <- crop(raster(miroc_max_2), e)/10
miroc_min_2 <- crop(raster(miroc_min_2), e)/10
miroc_c5_2 <- crop(raster(miroc_c5_2), e)/10



# Archivos a utilizar para no usar los 19 archivos, hacemos las medias de los modelos
minimas_40 <- (fio_min+acces_min+miroc_min)/3
maximas_40 <- (fio_max+acces_max+miroc_max)/3
medias_40 <- (fio_model+acces+miroc_c5)/3

minimas_60 <- (fio_min_2+acces_min_2+miroc_min_2)/3
maximas_60 <- (fio_max_2+acces_max_2+miroc_max_2)/3
medias_60 <- (fio_model2+acces2+miroc_c5_2)/3


#Observamos la distribucion del reno
points(reno$lat~reno$lon, pch=20, cex=0.6, col="red") 

#creo el stack de modelos
predictores <- stack(minimas_40, maximas_40, medias_40, minimas_60, maximas_60, medias_60)

plot(predictores)

# Elimino los valores de NO DATA del habitat del reno
range(na.omit(reno[,c("lon")]))
range(na.omit(reno[,c("lat")]))
# Hallo las coordenadas donde se encuentra el reno
coord <- reno[,c("lon", "lat")]
# Extraigo los valores dentro de mi modelo donde deberia de estar el reno
values <- extract(predictores, coord)
glimpse(values)

x11()
pairs(values) #Vemos la relacion entre las variables
# Creo un data frame de datos de los lugares donde esta el reno
data <- as.data.frame(values)
data <- na.omit(data) #Omitiendo los valores de no data
plot(data)

#Utilizamos la temperatura media, minima y max para estimar el modelo de 2040
datos <-  subset(predictores, c(1, 2, 3)) 
model1 <- bioclim(datos, coord) #bioclim crea el modelo
class(model1) # Vemos que el modelo tiene una configuracion propia de bioclim

##model prediction
map <- predict(datos, model1)
class(map)

##pasamos de idoneidad de habitat a presencia
presp <- reclassify(map,c(0,0.75,0,0.76,1,1)) #de 0 -0.75 sera 0, de 0.76 a 1, es 1
plot(presp) #suponemos que a partir de 0,75 la probabilidad es lo suficientemente
#alta para decir que habitan ahi los linces

#Observamos la distribucion del reno
points(reno$lat~reno$lon, pch=20, cex=0.6, col="red")

###AHORA CON LOS 60
#Utilizamos la temperatura media, minima y max para estimar el modelo de 2040
datos2 <-  subset(predictores, c(4, 5, 6)) 
model2 <- bioclim(datos2, coord) #bioclim crea el modelo
class(model2) # Vemos que el modelo tiene una configuracion propia de bioclim

##model prediction
map2 <- predict(datos2, model2)
class(map2)


##pasamos de idoneidad de habitat a presencia
presp2 <- reclassify(map2,c(0,0.75,0,0.76,1,1)) #de 0 -0.75 sera 0, de 0.76 a 1, es 1
plot(presp2) #suponemos que a partir de 0,75 la probabilidad es lo suficientemente
#alta para decir que habitan ahi los linces
#Observamos la distribucion del reno
points(reno$lat~reno$lon, pch=20, cex=0.6, col="red")
# finalmente se observa que en los 60 baja considerablemente el habitat donde podran habitar los renos