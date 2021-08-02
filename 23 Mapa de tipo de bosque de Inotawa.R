# Mapa de Ubicacion de tipo de bosque de Inotawa
#------------------------------------------------------------------------
require(pacman)
pacman::p_load(ggplot2,rgdal,ggspatial, raster, cowplot, egg, rnaturalearth, sf,hddtools,ggsn,ggpubr, yarrr,
               tibble,ggrepel,rnaturalearthdata  )
#-----------------------------------------------------------------------------------
#Cargar los datos Shp
Peru_n          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()       # Extracion del paiz
Sur_America     <- st_read ("Data shp/SurAmerica.shp")  
Tipo_Bosque     <-st_read("Data shp/Tipo_de_Bosque.shp")
Rios            <-st_read("Data shp/Rios.shp")
Agricola        <-st_read("Data shp/Agricola.shp")
Ino             <-st_read("Data shp/Inotawa.shp")
Ino2            <-st_read("Data shp/Inotawa2.shp")
SurAmerica_utm  <- st_transform(Sur_America,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Tipo_Bosque     <- st_transform(Tipo_Bosque,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rios            <- st_transform(Rios,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Agricola        <- st_transform(Agricola,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Ino             <- st_transform(Ino,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Ino2            <- st_transform(Ino2,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Marco_Tipo_Bosq = st_as_sfc(st_bbox(Tipo_Bosque ))


#-----------------------------------------------------------------------------------
#                        Colores para el panel
col <- c('#ffffff','#eaf1e7','#d4e2cf','#bfd4b8','#abc5a1','#96b78a',
         '#81a974','#6c9b5f','#588d4a','#428034','#28711d','#006400')
library(cartography)
col1 = carto.pal(pal1 = "green.pal", n1 = 15)
map.colors <- c(col,col1)
#-----------------------------------------------------------------------------------

MDD <-ggplot()+
  geom_sf(data= SurAmerica_utm, col = "grey80", fill = "grey80") +
  geom_sf(data= Peru_n, fill= NA, col="black")+
  geom_sf(data = Marco_Tipo_Bosq , fill=NA, color ="red")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect(linetype = "dashed", color = "grey20", fill = NA, size = 0.4))

MDD_bosque <-ggplot() +
  geom_sf(data = Tipo_Bosque,aes(fill = Simbolo),  alpha = 1, linetype = 1 )  +
  scale_fill_manual(values = map.colors)+
  ylab("Latitude") +
  xlab("Longitude") +
  annotate(geom = "text", x = -71, y = -9.5, label = "Tipo de Bosque en \nMadre de Dios", fontface = "italic", color = "black", size = 4)+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-73,-67.6), ylim = c(-13.5,-9),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        legend.title =element_text(size=10, face = "bold"), #tamaño de titulo de leyenda
        legend.text =element_text(size=8, face = "bold"),
        legend.position = c(.89, .25),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 16))+
        labs(fill = "Tipo de Bosque")+
        guides(fill = guide_legend(nrow = 28, ncol=1))
  
  
Ino_bosque <-ggplot() +
  geom_sf(data = Tipo_Bosque,aes(fill =CobVeg2013),  alpha = 1, linetype = 1, show.legend = FALSE )+
  scale_fill_manual(values = map.colors)+
  geom_sf(data=Agricola, fill=NA, color="Black")+
  geom_sf(data=Rios, fill=NA, color="blue")+
  geom_sf(data=Ino, fill=NA, color="red")+
  geom_sf(data=Ino2, fill=NA, color="red")+
  annotation_scale() +
  annotation_north_arrow(location="br",which_north="true",style=north_arrow_nautical ())+
  annotate(geom = "text", x = -69.285, y = -12.835, label = "Bosque de terraza baja", fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -69.282, y = -12.810, label = "Bosque de terraza \nalta con castaña", fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -69.302, y = -12.815, label = "Bosque de terraza baja", fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -69.295, y = -12.832, label = "Rios", fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -69.285, y = -12.820, label = "Inotawa", fontface = "italic", color = "red", size = 3)+
  annotate(geom = "text", x = -69.300, y = -12.813, label = "Inotawa", fontface = "italic", color = "red", size = 3)+
  coord_sf(xlim = c(-69.310,-69.275), ylim = c(-12.845,-12.805),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect(linetype = "dashed", color = "grey20", fill = NA, size = 0.4))
# conviértalo en un grob para insertarlo más tarde
MDD.grob        <- ggplotGrob(MDD)
Ino_bosque.grob <- ggplotGrob(Ino_bosque)

long.Dispersion<- c(-69.310, -69.275, -69.275, -69.310)
lat.Dispersion <- c(-12.845,-12.845, -12.805, -12.805)
group <- c(1, 1, 1, 1)
latlong.Dispersion <- data.frame(long.Dispersion, lat.Dispersion, group)

map.bound <- MDD_bosque + 
          geom_polygon(data= latlong.Dispersion , aes(long.Dispersion, lat.Dispersion, group=group), fill = NA, color = "red", 
               linetype = "dashed", size = 1, alpha = 0.8)

map.bound.inset <- map.bound + 
  annotation_custom(grob= MDD.grob, xmin = -73, xmax = -72, ymin =-10, ymax=-9) +
  annotation_custom(grob= Ino_bosque.grob, xmin = -70, xmax = -68, ymin =-11, ymax=-9) 

map.final <- map.bound.inset +
  geom_segment(aes(x=-72.62, xend=-72, y=-9.4, yend=-11),  linetype = "dashed", color = "grey20", size = 0.3) +
  geom_segment(aes(x=-72.6, xend=-70.7, y=-9.4, yend=-10), linetype = "dashed", color = "grey20", size = 0.3) +
  geom_segment(aes(x=-46, xend=-69, y=-13, yend=-30), linetype = "dashed", color = "grey20", size = 0.3) +
  geom_segment(aes(x=-69.3, xend=-69.8, y=-12.8, yend=-11), linetype = "dashed", color = "red", size = 1)+
  geom_segment(aes(x=-69.3, xend=-68.2, y=-12.8, yend=-11), linetype = "dashed", color = "red", size = 1)
#------------------------------------------------------------------------
ggsave(plot = map.final  ,"Mapas exportados/Tipo de bosque en Inotawa.png", units = "cm", 
       width = 29,height = 21, dpi = 900)




