##Shadows=group
##shadowHeight=name
##shpfile=file
##outdir=folder
##stylefile=file
##time1=string 2019-06-21 06:00:00
##time2=string 2019-06-21 23:00:00
##extbuffer=number 2000
##cadaXmin=number 60
##resolucion=number 5
##atlaslayers=string Astillero|Google Satellite

#librerias
library(shadow);library(raster);library(rgeos);library(rgdal);library(maptools);
library(parallel);library(ggplot2);library(igraph);library(threejs);library(lubridate);
library(rasterVis); library(gtools);library(dismo); library(googleVis); library(GISTools);
library(rgdal)

#parametros
shpfile <- "Z:/Proxectos/589_astillero_4_0/5_gis/paisaje/Sombreado/shp/conNave.shp"
outdir <- "Z:/Proxectos/589_astillero_4_0/5_gis/paisaje/Sombreado/_02_Asentamientos"
stylefile <- "Z:/De sastre/CAC/github/R_Shadows/styles/shadowHeight.qml"
time1 <- "2019-03-20 06:00:00"
time2 <- "2019-03-20 23:00:00"
extbuffer <- 1000
cadaXmin <- 120
resolucion <- 10
atlaslayers <- "Astillero|Google Satellite"

# Dia y hora para entroducir en herramientas shadow
t1 = as.POSIXct(time1, tz = "Europe/Paris")
t2 = as.POSIXct(time2, tz = "Europe/Paris")
s <- cadaXmin*60
dia <- substr(gsub('-','',toString(t1)),1,8)

#crear carpeta de destino con fecha de calculo
output <- paste(outdir,
                paste0(gsub('-','',dia),
                       '_shadowHeight',
                       '_',
                       toString(resolucion)),
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

# Raster base ajustar a extension del shp con 1000m de buffer
ext=as(raster::extent(shp)+extbuffer, "SpatialPolygons")
r = raster::raster(ext, res=resolucion)
proj4string(r) = proj4string(shp)

# Crear shapefile base para atlas con la extension del raster (add campos nombre y capas de atlas)
e <- extent(r)
shptemp <- as(e,'SpatialPolygons')
shpfinal <- as(e,'SpatialPolygons')
proj4string(shptemp) = proj4string(shp)
proj4string(shpfinal) = proj4string(shp)
shptemp$nombre <- "NA"
shptemp$layers <- "NA"
shpfinal$nombre <- "NA"
shpfinal$layers <- "NA"

#CALCULO DE ALTURA DE SOMBRA
#guardar archivo de visualizacion para qgis y crea lista de nombres de archivos
tiflist <- list()
while (t1<=t2) {
        # Calcular posicion solar para el dia y hora seleccionados
        solar_pos = maptools::solarpos(crds = location_geo,dateTime = t1)
        
        #superficie de sombras raster
        sombrasr = shadowHeight(
                location = r,
                obstacles = shp,
                obstacles_height_field = "Cota",
                solar_pos= solar_pos,
                #procesado paralelo (numero de cores)
                parallel = 3
        )
        #actualizar nombre de archivo y guardar
        nombre <- gsub(' ','_',gsub(':','',gsub('-','',toString(t1))))
        writeRaster(sombrasr, paste(output,nombre,sep='/'), format = "GTiff", overwrite=T)
        
        #guardar archivo de estilo con el mismo nombre del tif
        stylecopy <- paste(output,paste(nombre,'.qml',sep=''),sep='/')
        file.copy(from = stylefile, to = stylecopy)
        
        #actualizar shps para atlas (ojo en el orden de las capas)
        shptemp$nombre <- nombre
        shptemp$layers <- paste(nombre,"|",atlaslayers,sep='')
        shpfinal <- rbind(shpfinal,shptemp)
        
        #actualizar t1 para la siguiente iteracion
        t1 = t1+s
}

#crear lista de archivos .tif del directorio necesario para generar atlas de diferentes rasters
tifs <- list.files(output, full.names = T, pattern = ".tif$")
tifs2 <- list.files(output, full.names = F, pattern = ".tif$")
tifs3 <- substr(tifs2,1,nchar(tifs2)-4)
tiflist <- cbind(tifs, tifs2, tifs3)
colnames(tiflist) <- c("Path","Name_ext", "nombre")
tiflistfile <- paste(output,"tiflist.csv",sep='/')
write.table(tiflist, file=tiflistfile, sep=';')

#escribir el shpfinal de atlas para qgis uniendo los directorios de los archivos
shpfinal <- merge(x=shpfinal, y=tiflist, by="nombre", all=T)
writeOGR(obj=shpfinal,
         dsn=output,
         layer="atlas",
         driver="ESRI Shapefile",
         overwrite_layer = T)

#________________________________________
#CREAR RASTER TIME SERIES
#crear time raster series con raster stack
tifs.stack <- stack(tifs)
#aplicar factor de escala
tifs.stack <- tifs.stack

#solucionar el incorporar el siguiente shp en cada uno de los cuadros
#plot(shp, add=T, nc=2,col = adjustcolor("lightgrey", alpha.f = 0.5))

#exportar plots a pdf
file.pdf <- paste(output,'file.pdf',sep='/')
pdf(file.pdf, width=8, height=8, paper='special', pagecentre = T)
plot(tifs.stack,nc=2)
dev.off()
