##Shadows=group
##footprint=name
##shpfile=file
##outdir=folder
##stylefile=file
##time1=string 2019-12-21 06:00:00
##time2=string 2019-12-21 23:00:00
##extbuffer=number 2000
##cadaXmin=number 300
##atlaslayers=string Astillero|Google Satellite

#librerias
library(shadow);library(raster);library(rgeos);library(rgdal);library(maptools);
library(parallel);library(ggplot2);library(igraph);library(threejs);library(lubridate);
library(rasterVis); library(gtools);library(dismo); library(googleVis); library(GISTools);
library(rgdal); library(geom)

#parametros
shpfile <- "Z:/Proxectos/589_astillero_4_0/5_gis/paisaje/Sombreado/shp/conNave.shp"
outdir <- "Z:/Proxectos/589_astillero_4_0/5_gis/paisaje/Sombreado/_02_Asentamientos"
stylefile <- "Z:/De sastre/CAC/github/R_Shadows/styles/footprint_azul_rojo.qml"
time1 <- "2019-12-21 06:00:00"
time2 <- "2019-12-21 23:00:00"
extbuffer <- 1000
cadaXmin <- 30
atlaslayers <- "Astillero|Google Satellite"

# Dia y hora para entroducir en herramientas shadow
t1 = as.POSIXct(time1, tz = "Europe/Paris")
t2 = as.POSIXct(time2, tz = "Europe/Paris")
s <- cadaXmin*60
dia <- substr(gsub('-','',toString(t1)),1,8)

#crear carpeta de destino con fecha de calculo
output <- paste(outdir,
                paste0(gsub('-','',dia),
                       '_footprint'),
                sep='/')
dir.create(output)

#cargar y leer shapefile
shpdir <- dirname(shpfile)
shpname <- substr(basename(shpfile),1, nchar(basename(shpfile))-4)
shp <- readOGR(dsn = shpdir, layer=shpname)

#localizacion para calcular altura y azimuth
location = rgeos::gCentroid(shp)
#punto central del shp en coordenadas geograficas
location_geo = sp::spTransform(location,"+proj=longlat +datum=WGS84")

# Crear shapefile base para atlas con la extension del raster (add campos nombre y capas de atlas)
e <- extent(shp)+extbuffer
shptemp <- as(e,'SpatialPolygons')
shpfinal <- as(e,'SpatialPolygons')
proj4string(shptemp) = proj4string(shp)
proj4string(shpfinal) = proj4string(shp)
shptemp$nombre <- "NA"
shptemp$layers <- "NA"
shpfinal$nombre <- "NA"
shpfinal$layers <- "NA"

#CALCULO DE SOMBREADO
#guardar archivo de visualizacion para qgis y crea lista de nombres de archivos
shplist <- list()

#horas de salida y puesta para el día de cálculo
sunrise <- sunriset(location_geo, t1,direction="sunrise", POSIXct.out=TRUE)
sunset <- sunriset(location_geo, t1,direction="sunset", POSIXct.out=TRUE)
t1>=sunrise$time && t1<= sunset$time

while (t1<=sunset$time) {
        #si la hora de t1 aun es menor a la salida del sol no hacer nada y cambiar hora
        if (t1<=sunrise$time){
                t1=t1+s
        } else {
                # Calcular posicion solar para el dia y hora seleccionados
                solar_pos = maptools::solarpos(crds = location_geo,dateTime = t1)
                
                #Calculo de sombreado (visible o no visible)
                footprint <- shadowFootprint(
                        obstacles = shp,
                        obstacles_height_field = "Cota",
                        solar_pos = solar_pos
                )
                #add atributos de hora de calculo, sunrise y sunset
                footprint$Hora <- t1
                footprint$sunrise <- sunrise$time
                footprint$sunset <- sunset$time
                
                #actualizar nombre de archivo y guardar
                nombre <- gsub(' ','_',gsub(':','',gsub('-','',toString(t1))))
                #guardar el shp
                writeOGR(obj=footprint,
                         dsn=output,
                         layer=nombre,
                         driver="ESRI Shapefile",
                         overwrite_layer = T)
                #guardar archivo de estilo con el mismo nombre del shp
                stylecopy <- paste(output,paste(nombre,'.qml',sep=''),sep='/')
                file.copy(from = stylefile, to = stylecopy)
                
                #actualizar shps para atlas (ojo en el orden de las capas)
                shptemp$nombre <- nombre
                shptemp$layers <- paste(nombre,"|",atlaslayers,sep='')
                shpfinal <- rbind(shpfinal,shptemp)
                
                #actualizar t1 para la siguiente iteracion
                t1 = t1+s
        }
}

#crear lista de archivos .shp del directorio necesario para generar atlas
shps <- list.files(output, full.names = T, pattern = ".shp$")
shps2 <- list.files(output, full.names = F, pattern = ".shp$")
shps3 <- substr(shps2,1,nchar(shps2)-4)
shplist <- cbind(shps, shps2, shps3)
colnames(shplist) <- c("Path","Name_ext", "nombre")
shplistfile <- paste(output,"shplist.csv",sep='/')
write.table(shplist, file=shplistfile, sep=';')


#unir todos los shps de la lista
shpL1 <- shps[1]
shpdir1 <- dirname(shpL1)
shpname1 <- substr(basename(shpL1),1, nchar(basename(shpL1))-4)
shp0 <- readOGR(dsn = shpdir1, layer=shpname1)

for (x in 2:length(shps)){
        shpL2 <- shps[x]
        shpdir2 <- dirname(shpL2)
        shpname2 <- substr(basename(shpL2),1, nchar(basename(shpL2))-4)
        shp2 <- readOGR(dsn = shpdir2, layer=shpname2)
        shps.union <- rbind(shp0,shp2)
        shp0 <- shps.union
}
shp0name <- paste(substr(gsub('-','',toString(t1)),1,8),'_unido',sep='')
writeOGR(obj=shp0,
         dsn=output,
         layer=shp0name,
         driver="ESRI Shapefile",
         overwrite_layer = T)

#guardar archivo de estilo con el mismo nombre del shp
stylecopy <- paste(output,paste(shp0name,'.qml',sep=''),sep='/')
file.copy(from = stylefile, to = stylecopy)


#escribir el shpfinal de atlas para qgis uniendo los directorios de los archivos
shpfinal <- merge(x=shpfinal, y=shplist, by="nombre", all=T)
atlasname <- substr(gsub('-','',toString(t1)),1,8)
writeOGR(obj=shpfinal,
         dsn=output,
         layer=paste(atlasname,"atlas",sep='_'),
         driver="ESRI Shapefile",
         overwrite_layer = T)
