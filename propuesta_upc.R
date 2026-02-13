# PROPUESTA OPTIMIZADA DE UNIDADES DE POLICÍA COMUNITARIA EN RIOBAMBA BASADA EN COBERTURA DELICTIVA Y ANÁLISIS ESPACIAL (2020-2025)
# AUTORES: SEBASTIÁN HINOJOSA & FREDDY TOMALÁ

# 1. LIBRERÍAS----
library(sf)
library(dplyr)
library(stringr)
library(readxl)
library(tmap)
library(spatstat.geom)
library(spatstat.explore)
library(stars)
library(summarytools)
library(tidyr)

tmap_mode("plot")

# 2. CARGA DE DATOS----
barrios <- st_read("barrios1.shp") |>
  left_join(read_xlsx("poblacionxbarrios.xlsx"), by="BARRIO")

parroquias <- st_read("Parroquias_Urbanas.shp")

delitos <- read_excel("DELITOS 2020-NOV 2025.xlsx") |>
  st_as_sf(coords=c("X","Y"), crs=32717)

upc <- read_excel("upc_geo.xlsx") |>
  st_as_sf(coords=c("X","Y"), crs=32717)

parroquias <- st_transform(parroquias, st_crs(delitos))

# 3. FILTRO Y CATEGORIZACIÓN----
delitos <- delitos |>
  st_join(parroquias, join=st_within) |>
  st_join(barrios, join=st_within) |>
  filter(!is.na(BARRIO)) |>
  mutate(
    categoria_delito = case_when(
      str_detect(NewIncidentTypeName,"Homicidio|Asesinato|Sicariato|Secuestro|Violencia|Suicidio") ~ "Contra la persona",
      str_detect(NewIncidentTypeName,"Hurto|Robo|Estafa|Extorsión|Receptación") ~ "Contra la propiedad",
      str_detect(NewIncidentTypeName,"Violación|Abuso sexual|Acoso sexual") ~ "Delitos sexuales",
      str_detect(NewIncidentTypeName,"drogas|fiscalización") ~ "Drogas y salud pública",
      TRUE ~ "Otros"
    ),
    hora = as.numeric(format(as.POSIXct(incidentTime),"%H")),
    año  = as.numeric(format(as.POSIXct(incidentTime),"%Y")),
    franja = case_when(
      hora>=6 & hora<12 ~ "Mañana \n (06:00 -11:59)",
      hora>=12 & hora<18 ~ "Tarde \n (12:00 -17:59)",
      hora>=18 & hora<24 ~ "Noche \n (18:00 - 23:59)",
      TRUE ~ "Madrugada \n (00:00 - 05:59)"
    )
  )

# 4. MAPA GENERAL----
tm_shape(barrios)+
  tm_polygons(col="lightgray",alpha=0.2,border.col="black",lwd=2)+
  tm_shape(delitos)+
  tm_dots(col="darkblue",size=0.2,alpha=0.7)+
  tm_shape(upc)+
  tm_dots(col="darkorange2",size=0.7)

# 5. ESTADÍSTICA DESCRIPTIVA----
table(delitos$PARROQUIA)
table(delitos$categoria_delito)
table(delitos$año)
table(delitos$franja)

# Serie temporal anual
tab_anio <- table(delitos$año)

plot(names(tab_anio), as.numeric(tab_anio),
     type="o",col="darkred",pch=19,lwd=2,
     main="Evolución Temporal de Delitos en Riobamba",
     xlab="Año",ylab="Frecuencia",xaxt="n")

axis(1,at=names(tab_anio),labels=names(tab_anio))
text(names(tab_anio),as.numeric(tab_anio),
     labels=as.numeric(tab_anio),pos=3)

# Serie horaria
tab_hora <- table(delitos$hora)

plot(names(tab_hora),as.numeric(tab_hora),
     type="o",col="red",pch=19,lwd=2,
     main="Distribución Horaria de Delitos",
     xlab="Hora",ylab="Frecuencia",xaxt="n")

axis(1,at=names(tab_hora),labels=names(tab_hora))
text(names(tab_hora),as.numeric(tab_hora),
     labels=as.numeric(tab_hora),pos=3)

# Año x Categoría
tabla_ac <- table(delitos$año,delitos$categoria_delito)
anios <- as.numeric(rownames(tabla_ac))
colores <- rainbow(ncol(tabla_ac))

plot(anios,tabla_ac[,1],type="o",col=colores[1],
     pch=19,lwd=2,ylim=c(0,max(tabla_ac)),
     main="Evolución temporal de delitos por categoría",
     xlab="Año",ylab="Número de delitos")

for(i in 2:ncol(tabla_ac)){
  lines(anios,tabla_ac[,i],type="o",
        col=colores[i],pch=19,lwd=2)
}

legend("topleft",legend=colnames(tabla_ac),
       col=colores,lwd=2,pch=19,cex=.8,bty="n")

# 6. MAPA DE CALOR KDE----
area <- st_union(barrios)
win  <- as.owin(area)
pp   <- ppp(st_coordinates(delitos)[,1],
            st_coordinates(delitos)[,2],
            window=win)

dens <- density(pp,sigma=400,edge=TRUE)
ras  <- st_as_stars(dens); st_crs(ras)=st_crs(barrios)
ras  <- ras[area]

tm_shape(ras)+
  tm_raster(palette=colorRampPalette(c("darkgreen","yellow","red"))(256),
            alpha=.85,title="Intensidad de delitos")+
  tm_shape(barrios)+tm_borders(col="black",lwd=1.5)+
  tm_shape(upc)+tm_dots(col="black",size=.7)

# 7. DISTANCIAS Y BUFFER----
dist_mat <- st_distance(delitos,upc)
delitos$dist_min_upc <- apply(dist_mat,1,min)
summary(delitos$dist_min_upc)
descr(delitos$dist_min_upc)

buffer_upc <- st_buffer(upc,448)

tm_shape(barrios)+tm_polygons(col="lightgray",alpha=.2,border.col="black",lwd=2)+
  tm_shape(buffer_upc)+tm_polygons(col="orange",alpha=.3,border.col="darkorange2")+
  tm_shape(delitos)+tm_dots(col="darkblue",size=.2)+
  tm_shape(upc)+tm_dots(col="darkorange2",size=.7)

delitos$en_buffer <- apply(st_within(delitos,buffer_upc,sparse=FALSE),1,any)
delitos_fuera <- delitos |> filter(!en_buffer)

# 8. TASA DELICTIVA----
conteo <- delitos |> st_drop_geometry() |> count(BARRIO,name="Conteo_delitos")

barrios <- barrios |> left_join(conteo,by="BARRIO") |>
  mutate(Conteo_delitos=replace_na(Conteo_delitos,0),
         tasa_delictiva=(Conteo_delitos/Población)*1000)

tm_shape(barrios)+
  tm_polygons(col="tasa_delictiva",
              palette=colorRampPalette(c("lightyellow","orange","red"))(5),
              style="quantile",
              alpha=.8,
              border.col="black",
              lwd=2,
              title="Tasa delictiva\n(por barrio)")+
  tm_shape(upc)+tm_dots(col="black",size=.7)

# 9. ÍNDICE ESPACIAL----
centroides <- st_centroid(barrios)
centroides$dist_media_upc <- apply(st_distance(centroides,upc),1,min)
centroides$riesgo_espacial <- centroides$tasa_delictiva * centroides$dist_media_upc

# 10. K-MEANS----
datos_ml <- centroides |>
  st_drop_geometry() |>
  select(tasa_delictiva,dist_media_upc,riesgo_espacial) |>
  scale()

set.seed(16)
k3 <- kmeans(datos_ml,centers=3,nstart=100)

centroides$cluster_riesgo <- factor(
  k3$cluster,
  labels=c("Bajo riesgo","Riesgo medio","Alto riesgo")
)

clasificacion_barrio <- centroides |>
  st_drop_geometry() |>
  select(BARRIO,cluster_riesgo,
         tasa_delictiva,dist_media_upc,riesgo_espacial)

barrios <- barrios |>
  left_join(clasificacion_barrio,by="BARRIO")

table(barrios$cluster_riesgo)

aggregate(cbind(tasa_delictiva,
                dist_media_upc,
                riesgo_espacial)~cluster_riesgo,
          data=st_drop_geometry(centroides),mean)

paleta_riesgo <- c("Bajo riesgo"="#00CD66",
                   "Riesgo medio"="#EEEE00",
                   "Alto riesgo"="#CD0000")

tm_shape(barrios)+
  tm_polygons(col="cluster_riesgo",
              palette=paleta_riesgo,
              alpha=.8,
              border.col="black",
              lwd=2,
              title="Nivel de riesgo\ndelictivo")+
  tm_shape(upc)+tm_dots(col="black",size=.7)


# 11. PROPUESTA NUEVAS UPC----
upc$delit_cubiertos <- lengths(st_intersects(buffer_upc,delitos))
umbral <- median(upc$delit_cubiertos)

coords <- st_coordinates(delitos_fuera)
set.seed(123)
k <- kmeans(coords,12,nstart=25)
delitos_fuera$cluster <- k$cluster

cand <- delitos_fuera |>
  group_by(cluster) |>
  summarise(geometry=st_centroid(st_union(geometry))) |>
  ungroup()

buf_cand <- st_buffer(cand,436)
cand$delit_cubiertos <- lengths(st_intersects(buf_cand,delitos_fuera))

upc_nuevas <- cand |> filter(delit_cubiertos>=umbral)

upc_nuevas_ord <- upc_nuevas |>
  arrange(delit_cubiertos) |>
  mutate(Prioridad = factor(row_number(),
         levels = c(1, 2, 3, 4),
         labels = c("4", "3", "2", "1")))

tm_shape(barrios)+tm_polygons(col="lightgray",alpha=.3,border.col="black",lwd=2)+
  tm_shape(delitos_fuera)+tm_dots(col="darkblue",size=.25)+
  tm_shape(upc)+tm_dots(col="darkorange1",size=.7)+
  tm_shape(upc_nuevas_ord)+tm_dots(col="red",size=1.8)+
  tm_text("Prioridad", col="white", size=.9, fontface="bold")
