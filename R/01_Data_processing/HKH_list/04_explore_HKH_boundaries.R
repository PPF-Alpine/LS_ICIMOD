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


### ggplot 
library(ggplot2)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

# 1. Get country polygons and filter for Asia
world <- ne_countries(scale = "medium", returnclass = "sf")
asia <- world %>% 
  filter(region_un == "Asia")

# 2. Set custom extent (bounding box for HKH)
# Adjust as needed to focus more or less
hkh_bbox <- st_bbox(c(xmin = 62, xmax = 105, ymin = 17, ymax = 40), crs = st_crs(asia))

# 3. Create color palette for GMBA
gmba_colors <- brewer.pal(n = length(unique(hkh_gmba_subset$MapName)), "Set2")

# 4. Plot
ggplot() +
  # Base: Asian countries
  geom_sf(data = asia, fill = "gray95", color = "gray80", size = 0.3) +
  
  # GMBA polygons
  geom_sf(data = hkh_gmba_subset, aes(fill = MapName), color = "black", size = 0.1, alpha = 0.6) +
  
  # HKH boundary as legendable layer
  geom_sf(data = hkh_boundaries, aes(color = "HKH Boundary"), fill = NA, size = 1) +
  
  # Country names
  geom_text(data = st_centroid(asia), aes(x = st_coordinates(geometry)[,1], 
                                          y = st_coordinates(geometry)[,2], 
                                          label = name), size = 3, color = "gray20") +
  
  # Legends
  scale_fill_manual(values = gmba_colors,name=NULL) +
  scale_color_manual(name = "", values = c("HKH Boundary" = "darkblue")) +
  
  # Map theme and extent
  coord_sf(xlim = c(hkh_bbox["xmin"], hkh_bbox["xmax"]),
           ylim = c(hkh_bbox["ymin"], hkh_bbox["ymax"])) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) 

plot_output_path <- paste0(data_storage_path, "Visualizations/")

ggsave(
  filename = paste0(plot_output_path, "hkh_gmba_map.jpg"),
  width = 10,
  height = 6,
  dpi = 300
)
