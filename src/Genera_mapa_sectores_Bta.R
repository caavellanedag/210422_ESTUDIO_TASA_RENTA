require(pacman)
p_load(gstat, geoR, RColorBrewer, leaflet, 
       leaflet.extras, sp, rgdal, sf, reshape2, maptools, rgeos, htmlwidgets)

Sectores_shp <- sf::st_read("input/Base_Dic2020/Sectores/Sectores1712.shp")
Sectores_shp <- Sectores_shp %>% as('Spatial')  %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84")) 
Bogota_map <- Sectores_shp %>%   leaflet(options = leafletOptions(dragging = TRUE, minZoom = 7,maxZoom = 18)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(stroke = TRUE,weight = 2,fillOpacity =0,dashArray = "3", label = Sectores_shp@data$SECSECT_ID)

saveWidget(Bogota_map,
           file = paste0("output/", str_replace_all(Sys.Date(),c("^2021"="21","-"="")), "_Mapa_sectores.html"), 
           selfcontained = F)