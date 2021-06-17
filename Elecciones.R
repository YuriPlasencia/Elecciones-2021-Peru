
#------------------------------------#
#-----Cambiando el Directorio--------#
#------------------------------------#
# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
wd

# Limpiamos la consola
cat("\014")
# dataMapas <- read.csv("http://www.minem.gob.pe/descripcion.php?idSector=10&idTitular=6040")
# dataMapasDistrito <- read.csv("http://geoservidorperu.minam.gob.pe/geoservidor/download.aspx")
# dataMapasMINEDU <- read.csv("http://sigmed.minedu.gob.pe/descargas/")



library(sf)
library(purrr)
library(tidyverse)

library(ggplot2)
library(ggrepel)
library(readxl)



peru_d <- st_read("BAS_LIM_DISTRITOS.shp") #Este comando permite leer el shapefile y 'transformarlo' en un data frame
head(peru_d,2)

#Centroides: Podemos crear un punto al centro de cada unidad, lo cual nos permitirá colocar el nombre de cada departamento
#Se crea el centroide
peru_d <- peru_d %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                   2))

#Mapa con etiquetas de departamentos
ggplot(data = peru_d) +
  geom_sf(fill="skyblue", color="black")+ #Se le agrega un relleno celeste y bordes negros
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP), size = 2.25) #Se inserta el nombre de cada departamento

#------------------------------------#
#BASES
#------------------------------------#
library(data.table)
library(readr)
BC<-read.csv(paste(wd,"/result_resumen_por_ubigeo.csv",sep = ""), header = T)
head(BC)


# BC <- BC[sample(1:nrow(BC), 10000, replace=FALSE),]
save.image(paste(wd,"/bases.RData",sep = ""))
# load(paste(wd,"/bases.RData",sep = ""))

#------------------------------------#
# DEFINIMOS FUNCIONES
#------------------------------------#

'%!in%' <- function(x,y)!('%in%'(x,y))
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}





library(ggplot2)
library(dplyr)

#------------------------------------------------------------------------------
# FILTROS
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# DATOS AGREAGADOS POR DEPARTAMENTO
#------------------------------------------------------------------------------

SDB <- BC
GRUPO <- dplyr::group_by(SDB,distrito)
TABSDB <- dplyr::summarise(GRUPO
                           ,PL = sum(votos_pl)/sum(votos_validos)
                           ,FP = sum(votos_fp)/sum(votos_validos)
                           ,n = n()
                           
)
TABSDB <- as.data.frame(TABSDB)
TABSDB <- subset(TABSDB, !is.na(distrito))
TABSDB


colnames(TABSDB)[1] <- c("NOMBDIST")
TABSDB




# install.packages("tmap")
library(sf)
library(raster)
library(dplyr)
library(spData)
# library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

# Add fill layer to nz shape
p1 <- tm_shape(peru_datos) +
  tm_fill("PL",title = "% Votantes de Izquierda", style = "quantile", n = 10,palette = c("#FF9900","#FF0000"), legend.hist = FALSE )+
  tm_shape(peru_datos)+
  tm_bubbles(size="n", title.size = "Número de Actas",col = "black")+
  tm_compass(position = c("left", "top"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels="%Votos PL vs FP",legend.outside=TRUE)+
  # tm_text("NOMBDEP", size= 0.55)+
  tm_graticules(n.x=4,n.y=4, lines = FALSE, labels.rot = c(0, 90))

tmap_save(p1, file = "PERU_map_01.png")



p1 <- tm_shape(peru_datos) +
  tm_fill("PL",title = "% Votantes de Izquierda", style = "quantile", n = 10,palette = c("#FF9900","#FF0000"), legend.hist = FALSE )+
  # tm_shape(peru_datos)+
  # tm_bubbles(size="n", title.size = "Número de Actas",col = "black")+
  tm_compass(position = c("left", "top"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels="%Votos PL vs FP",legend.outside=TRUE)+
  # tm_text("NOMBDEP", size= 0.55)+
  tm_graticules(n.x=4,n.y=4, lines = FALSE, labels.rot = c(0, 90))

tmap_save(p1, file = "PERU_map_02.png")



