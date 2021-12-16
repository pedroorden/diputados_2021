#librerias que vamos a usar
library(jsonlite)  
library(sf)
library(fuzzyjoin)
library(tidyverse)  
library(leaflet)
library(reshape)


####Votos####


#extraemos los datos de los votos de la carpeta circuitos


#abre la carpeta circuitos y genera la ruta para extraer
rutas <- list.files("circuitos", pattern = ".json$", full.names = TRUE, recursive = TRUE)


#funcion para levantar los archivos
contenido <- lapply(rutas, jsonlite::fromJSON)


#extracción con funcion y ruta
names(contenido) <- rutas


n.obs <- sapply(contenido, length)


seq.max <- seq_len(max(n.obs)) #estructura matrix con 4 calumnas generales


matriz <- t(sapply(contenido, "[", i = seq.max)) #abre la lista


dataframe <- melt(matriz) #atención, pivotam la data


dataframe <- dataframe %>% 
  unnest(value.valoresTotalizadosPositivos)%>%
  as.data.frame()#abre datos positivos


votospositivos <- dataframe%>%
dplyr::select(X1, nombreAgrupacion, votos, votosPorcentaje)# arma el df de votos positivos


#editamos el nombre del archivo para generar un identificador del circuito por provincia
votos<-votospositivos%>%
  mutate(X1=gsub('circuitos/','',X1))%>% #quita circuitos/
  mutate(X1=gsub('.json','',X1)) #quita .json


votos <- votos%>%
  mutate(n_provincia=substr(X1, 1, 2)) #definimos provincia para filtrar


votos$X1 <- gsub("^.{0,7}", "", votos$X1) #editamos el id del circuiro para joinear


votos$circuito <- votos$X1


votos <- votos %>% select(!X1) #soltamos X1


votos<-votos%>%
  distinct() #eliminamos los valores duplicados de la primera salida

# Fin de la descarga, generamos un df de todas las provincias en un formato más accesible


# Guardamos el df final con la totalidad de nuestros datos
saveRDS(votos, "votos.rds")

#seleccionamos los votos de ba
votos_ba<-votos%>%
  filter(n_provincia=="02")


#### Geolocalización ####


#Primera iteración de georefereciamiento


#Descargamos los datos de circuitos para la Provincia de Buenos Aires
#Fuente: Datos abiertos PBA
geo_ba<-read_sf("https://catalogo.datos.gba.gob.ar/dataset/4fe68b69-c788-4c06-ac67-26e4ebc7416b/resource/35322bb6-72f5-44ae-81eb-1686ca473e06/download/circuitos-electorales.geojson")


geo_ba<-geo_ba%>%
  #st_drop_geometry()%>%
  mutate(id=as_factor(circuito_id))


#generamos los corredores
circuitos_geo <- geo_ba%>%
  mutate(region=case_when(
    municipio_nombre %in%
      c("General San Martín", "Malvinas Argentinas", 
        "San Fernando", "San Isidro", "San Miguel", 
        "Tigre", "Tres de Febrero",
        "Vicente López")~"Corredor Norte",
    municipio_nombre %in%
      c("Hurlingham", "Ituzaingó", "José C. Paz", 
        "La Matanza", "Merlo", "Moreno" , 
        "Morón")~ "Corredor Oeste",
    municipio_nombre %in%
      c("Lanús", "Almirante Brown", "Lomas de Zamora",
        "Avellaneda", "Berazategui", "Quilmes",
        "Esteban Echeverría", "Florencio Varela", "Ezeiza")~"Corredor Sur",
    TRUE ~ "Otras"))%>%
  filter(!region=="Otras")%>%
  dplyr::rename(circuito=circuito_id)


#Estrategia de filtrado duro de los datos de 4 y 5 caracteres


#georeferencia circuitos de 5 caracteres
votos_5 <- circuitos_geo%>%
  group_by(circuito)%>%
  left_join(votos_ba, by="circuito")%>%
  drop_na()


#georef de circuitos de 4 caracteres
votosn4<-votos_ba%>%
  mutate(n_4=substr(circuito, 2, 5))%>%
  drop_na()


votos_4 <- votosn4%>%
  left_join(circuitos_geo, by=c("n_4"="circuito"))%>%
  drop_na()


chec2<-votosn4%>%
  group_by(circuito)%>%
  count()


bavotos_regiones<-votos_5%>%
  full_join(votos_4)%>%
  st_as_sf()


#install.packages("mapview")


mapview::mapview(bavotos_regiones)


saveRDS(bavotos_regiones, "bavotos_regiones.rds")


#Caso de USO de la data:





