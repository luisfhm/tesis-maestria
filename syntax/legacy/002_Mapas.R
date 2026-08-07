# Load libraries
library(ggplot2)
library(sf)

cdmx <- st_read(paste0(datadir,"colonias_iecm.shp"))


#Lectura de incidentes
incidentes<-readRDS(file = paste0(datadir,"Incidentes.RDS"))
incidentes<- incidentes %>% filter(year(fecha_creacion)>2019)
coords <- incidentes %>% select(latitud,longitud)
coords <- na.omit(coords)
set.seed(61)
points_sf <- st_as_sf(coords,coords = c("longitud","latitud"),crs=4326)

ggplot() + geom_sf(data=cdmx)+
  stat_density2d(data =points_sf, aes(x = st_coordinates(geometry)[,1],
                                       y = st_coordinates(geometry)[,2], 
                                       fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01, bins = 16) +
    scale_fill_gradient(low = "green", high = "red",guide = "none") +
    scale_alpha(range = c(0, 0.3),guide=F)+xlab("")+ylab("")+
  ggtitle("Distribución de accidentes viales\nen la Ciudad de México")
  
rm(incidentes)

#Lectura de infracciones
infracciones<-readRDS(file = paste0(datadir,"Infracciones.RDS"))
infracciones<- infracciones %>% filter(!is.na(latitud)|!is.na(longitud))
coords <- infracciones %>% select(latitud,longitud)
coords_sample<- coords[sample(1:nrow(coords),size =1000000,replace=F),]
points_sf <- st_as_sf(coords_sample,coords = c("longitud","latitud"),crs=4326)

ggplot() + geom_sf(data=cdmx)+
  stat_density2d(data =points_sf, aes(x = st_coordinates(geometry)[,1],
                                      y = st_coordinates(geometry)[,2], 
                                      fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01, bins = 16) +
  scale_fill_gradient(low = "green", high = "red",guide = "none") +
  scale_alpha(range = c(0.5, 0.7),guide=F)+xlab("")+ylab("")+
  ggtitle("Distribución de infracciones\nen la Ciudad de México")

rm(infracciones)


#Corregidas
infracciones<-readRDS(file = paste0(datadir,"Infracciones_corregida.RDS"))
infracciones<- infracciones %>% filter(!is.na(latitud)|!is.na(longitud))
coords <- infracciones %>% select(latitud,longitud)
coords_sample<- coords[sample(1:nrow(coords),size =1000000,replace=F),]
points_sf <- st_as_sf(coords_sample,coords = c("longitud","latitud"),crs=4326)

ggplot() + geom_sf(data=cdmx)+
  stat_density2d(data =points_sf, aes(x = st_coordinates(geometry)[,1],
                                      y = st_coordinates(geometry)[,2], 
                                      fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01, bins = 16) +
  scale_fill_gradient(low = "green", high = "red",guide = "none") +
  scale_alpha(range = c(0.5, 0.7),guide=F)+xlab("")+ylab("")+
  ggtitle("Distribución de infracciones\nen la Ciudad de México")

rm(infracciones)


incidentes<-readRDS(file = paste0(datadir,"Incidentes_corregida.RDS"))
incidentes<- incidentes %>% filter(year(fecha_creacion)>2019)
coords <- incidentes %>% select(latitud,longitud)
coords <- na.omit(coords)
set.seed(61)
points_sf <- st_as_sf(coords,coords = c("longitud","latitud"),crs=4326)

ggplot() + geom_sf(data=cdmx)+
  stat_density2d(data =points_sf, aes(x = st_coordinates(geometry)[,1],
                                      y = st_coordinates(geometry)[,2], 
                                      fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01, bins = 16) +
  scale_fill_gradient(low = "green", high = "red",guide = "none") +
  scale_alpha(range = c(0, 0.3),guide=F)+xlab("")+ylab("")+
  ggtitle("Distribución de accidentes viales\nen la Ciudad de México")

rm(incidentes)