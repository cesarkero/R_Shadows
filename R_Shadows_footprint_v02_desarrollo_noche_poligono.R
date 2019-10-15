
#librerias
library(shadow);library(raster);library(rgeos);library(rgdal);library(maptools);
library(parallel);library(ggplot2);library(igraph);library(threejs);library(lubridate);
library(rasterVis); library(gtools);library(dismo); library(googleVis); library(GISTools);
library(rgdal)

#parametros
shpfile <- "Z:/Proxectos/589_astillero_4_0/5_gis/shp/20180529_Astilleo_nuevo/Nave_principal.shp"
outdir <- "Z:/Proxectos/589_astillero_4_0/5_gis/paisaje/Sombreado/_01_Astillero_nuevo"
stylefile <- "Z:/De sastre/CAC/github/R_Shadows/styles/footprint.qml"
time1 <- "2019-12-21 23:00:00"
time2 <- "2019-12-21 23:00:00"
extbuffer <- 2000
cadaXmin <- 300
atlaslayers <- "Astillero|Google Satellite"

# Dia y hora para entroducir en herramientas shadow
t1 = as.POSIXct(time1, tz = "Europe/Paris")
t2 = as.POSIXct(time2, tz = "Europe/Paris")
s <- cadaXmin*60
dia <- substr(gsub('-','',toString(t1)),1,8)


#cargar y leer shapefile
shpdir <- dirname(shpfile)
shpname <- substr(basename(shpfile),1, nchar(basename(shpfile))-4)
shp <- readOGR(dsn = shpdir, layer=shpname)
crs(shp)
plot(shp)

#localizacion para calcular altura y azimuth
location = rgeos::gCentroid(shp)
#punto central del shp en coordenadas geograficas
location_geo = sp::spTransform(location,"+proj=longlat +datum=WGS84")

solar_pos = maptools::solarpos(crds = location_geo,dateTime = t1)

#comparar horas
sunrise <- sunriset(loc, t1,direction="sunrise", POSIXct.out=TRUE)
sunset <- sunriset(loc, t1,direction="sunset", POSIXct.out=TRUE)

p1 <- c(e[1],e[3])
p2 <- c(e[2],e[3])
p3 <- c(e[1],e[4])
p4 <- c(e[2],e[4])
puntos <- c(p1,p2,p3,p4)

px <- c(e[1],e[2],e[1],e[2])
py <- c(e[3],e[4],e[3],e[4])
poligono <- cbind(matrix(px),py)

p = Polygon(poligono)

ps = Polygons(list(p),1)
ps
sps = SpatialPolygons(list(ps))
crs(sps)=crs(shp)
plot(sps)
