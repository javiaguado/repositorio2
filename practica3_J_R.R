## PRACTICA 3 ####
# Archivo: practica3_J_R.R
# Autores: Javier Aguado y Ricardo Gascunana
# 12/03/2020
# Descripcion: El 'script' corresponde a la tercera practica de la asignatura de programacion
# avanzada en el master T.I.G. de la UAH, la tarea principal es la ejecucion de codigo manejando
# variables climaticas.

# Se debe de establecer el repositorio donde se quiean introducir los archivos 
# mediante 'setwd()'

library(rgdal)
library(raster)
library(dismo)
library(XML)
library(maptools)
library(dplyr)
library(sf)

## Pasos previos a la descarga de archivos sin 'hardcode' ####
# URL de la web de datos 'chelsa'
chelsa <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/"

# Links para las temeprtauras de los dos periodos
cuarenta <- "cmip5/2041-2060/temp/"
sesenta <- "cmip5/2061-2080/temp/"
decadas_url <- c(cuarenta, sesenta)

decadas <- c("2041-2060", "2061-2080")

# Siglas que representan los distintos modelos en la descarga
modelos_url <- c("MIROC5", "ACCESS1-0", "FIO-ESM")

# Archivos 2040-2060
fio_model <-  "FIO_SM.tif"
acces <- "ACCESS1.tif"
miroc_c5 <- "MIROC_5.tif"
archivos <- c(fio_model, acces, miroc_c5)


# Descarga de los 6 archivos a utilizar, tres modelos 2041-2060 y otros tres 2061-2080
cont <- 0
cont2 <- 0
for(i in decadas_url){
  cont <- cont+1
  for(j in modelos_url){
    cont2 <- cont2+1
    if(cont2<=3){archivo <- archivos[cont2]}
    else{archivo <- paste("60",archivos[cont2-3], sep = "")}
  
    download.file(url = paste(chelsa, i, "CHELSA_tas_mon_",
                j, "_rcp45_r1i1p1_g025.nc_1_", decadas[cont], "_V1.2.tif", sep=""),
                destfile = archivo, method = "curl")
    }}
cont <- 0  

# Archivos 2060-2080
fio_model2 <-  "60FIO_SM.tif"
acces2 <- "60ACCESS1.tif"
miroc_c5_2 <- "60MIROC_5.tif"

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

# Comprobamos la proyeccion para establecer posteriormente los 'clips'
# y empezar las operaciones
st_crs(temp_acces)
# Observamos que las tres coinciden
st_crs(temp_acces) == st_crs(temp_fio) %>% st_crs(temp_miroc)

# Recorte de cada mapa para la zona de Suecia.
e <- extent(10, 25, 55, 72)
# Predicciones para el periodo entre el 40 y el 60

# Se divide entre 10 segun indicaciones de "Chelsa" para obtener los grados centígrados
pred_acces <- crop(temp_acces, e)/10
pred_fio <- crop(temp_fio, e)/10
pred_miroc <- crop(temp_miroc, e)/10

# Predicciones para el periodo entre el 60 y el 80
pred_acces2 <- crop(temp_acces2, e)/10
pred_fio2 <- crop(temp_fio2, e)/10
pred_miroc2 <- crop(temp_miroc2, e)/10


# Se crean 2 mapas con la media de los modelos escogidos previamente.
# Map del periodo 40-60
media_modelos <- mean(pred_acces, pred_fio, pred_miroc)

# Mapa del periodo 60-80
media_modelos2 <- mean(pred_acces2, pred_fio2, pred_miroc2)

# Medias
par(mfrow=c(2,2))
plot(media_modelos, main="Tº media (2041-2060)", xlab= "Longitud (º)",
     ylab="Latitud (º)")

plot(media_modelos2, main="Tº media (2061-2080)", xlab= "Longitud (º)",
     ylab="Latitud (º)")

# Histograma que muestra como varian las temepraturas entre ambas epocas
# Se pasa a grados Kelvin para evitar valores negativos.
kelvin_media1 <- media_modelos + 273.15
kelvin_media2 <- media_modelos2 + 273.15
kelvin_media3 <- kelvin_media2 - kelvin_media1
hist(kelvin_media3, main="Pixeles que han aumentado o disminuido su Tº",
     ylab="Nº de pixeles", xlab="Tº aumentada o disminuida", col= "purple")

# Representacion numérica de la variacion

# Numero de pixeles donde la temperatura ha aumentado
length(kelvin_media3@data@values[kelvin_media3@data@values>0])

# Numero de pixeles donde la temperatura ha disminuido
length(kelvin_media3@data@values[kelvin_media3@data@values<0])


# Se analiza como varian los modelos respecto a la media.
# Las temperaruras son pasadas a Kelvin para evitar los valores negativos.
# Variacion para periodo 40-60
variacion_modelo1 <- (pred_acces+273.15)*100/(media_modelos +273.15)-100
variacion_modelo2 <- (pred_fio+273.15)*100/(media_modelos +273.15)-100
variacion_modelo3 <- (pred_miroc+273.15)*100/(media_modelos +273.15)-100

# Variacion para periodo 60-80
variacion_modelo1_2 <- (pred_acces2+273.15)*100/(media_modelos2 +273.15)-100
variacion_modelo2_2 <- (pred_fio2+273.15)*100/(media_modelos2 +273.15)-100
variacion_modelo3_2 <- (pred_miroc2+273.15)*100/(media_modelos2 +273.15)-100

## Histogramas de las variaciones:####

par(mfrow=c(3,2))

## Modelo Acces ####

# Histograma 2040-2060.
# Sus temperaturas son más bajas respecto a la media (Menos efecto del cambio climatico).
hist(variacion_modelo1, main="Acces. 2040-2060", col="blue", ylab="Numero de pixeles",
     xlab="Porcentaje de variacion %")

# Histograma 2060-2080.
# Observamos que para estas decadas, el modelo Acces es el mas concorde respecto a la media
# a diferencia de lo ocurrido en la decada 40-60
hist(variacion_modelo1_2, main="Acces. 2060-2080", col="blue", ylab="Numero de pixeles",
     xlab="Porcentaje de variacion %")

## Modelo FIO ####

# Histograma 2040-2060
# Todas las temperaturas son superiores a la media, siendo el modelo mas catastrofista.
hist(variacion_modelo2,main="FIO. 2040-2060", col="yellow", ylab="Numero de pixeles",
     xlab="Porcentaje de variacion %")

# Histograma 2060-2080 
# Al igual que lo ocurrido en la 40-60 este modelo presenta unas temperaturas realmente altas.
hist(variacion_modelo2_2,main="FIO. 2060-2080", col="yellow", ylab="Numero de pixeles",
     xlab="Porcentaje de variacion %")

## Modelo Miroc ####

# Histograma 2040-2060
# Histograma que más se ajusta a la media entre los modelos,
# aunque bajo efecto del cambio climatico respecto a la media.
hist(variacion_modelo3,main="Miroc. 2040-2060", col="chocolate", ylab="Numero de pixeles",
     xlab="Porcentaje de variacion %")

# Histograma 2060- 2080
# Al igual que le ocurría al modelo 'Acces', este modelo cambia respecto a la media vista
# en 2040-2060 siendo este el que ofrece unas temperaturas menores.
hist(variacion_modelo3_2,main="Miroc. 2060-2080", col="chocolate", ylab="Numero de pixeles",
     xlab="Porcentaje de variacion %")

# Una vez comparados los datos de los tres modelos, establecemos el modelo 'Miroc'
# para las decadas 2040-2060, y el 'Acces' para 2060-2080 como los mas cercanos a la media

