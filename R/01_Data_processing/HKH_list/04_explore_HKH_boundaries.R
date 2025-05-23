library(leaflet)
library(htmlwidgets)

hkh_gmba <- sf::st_read(paste(data_storage_path,"HKH_Boundary/HKH_GMBA_clip.shp", sep = "/"))

hkh_gmba_mountains <- c("Himalaya",
                        "Hindu Kush",
                        "Balochistan Ranges",
                        "Karakoram",
                        "Tibetan Plateau")

hkh_gmba_subset <- hkh_gmba|>
  filter(MapName%in%hkh_gmba_mountains)

hkh_boundaries <- sf::st_read(paste(data_storage_path,"HKH_Boundary/HKH_Boundary.shp", sep = "/"))


pal <- colorFactor(palette = "Set2", domain = hkh_gmba$MapName)

# leaflet map to check out HKH boundary

leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Map") %>%
  addPolygons(data = hkh_boundaries,
              color = "darkblue",
              weight = 2,
              fillOpacity = 0.1,
              group = "HKH Boundaries") %>%
  addPolygons(data = hkh_gmba_subset,
              fillColor = ~pal(MapName),
              color = "black",
              weight = 1,
              fillOpacity = 0.6,
              popup = ~MapName,
              group = "HKH GMBA") %>%
  addLegend("bottomright",
            pal = pal,
            values = hkh_gmba_subset$MapName,
            title = "GMBA names") %>%
  addLayersControl(
    overlayGroups = c("HKH Boundaries", "HKH GMBA"),
    options = layersControlOptions(collapsed = FALSE)
  )


