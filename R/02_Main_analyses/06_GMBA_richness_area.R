
# Load configuration file
source(here::here("R/00_Config_file.R"))

plot_output_path <- paste0(data_storage_path, "Visualizations/")
#----------------------------------------------------------#
# read assesment -----
#----------------------------------------------------------#

library(tidyverse)
library(patchwork)
library(sf)
library(rnaturalearth)
library(RColorBrewer)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_08062025.csv"))

hkh_boundaries <- sf::st_read(paste(data_storage_path,"HKH_Boundary/HKH_Boundary.shp", sep = "/"))


# read shps
hkh_gmba <- sf::st_read(paste(data_storage_path,"HKH_Boundary/HKH_GMBA_clip.shp", sep = "/"))

hkh_gmba_mountains <- c("Himalaya",
                        "Hindu Kush",
                        "Balochistan Ranges",
                        "Karakoram",
                        "Tibetan Plateau")

hkh_gmba_subset <- hkh_gmba|>
  filter(MapName%in%hkh_gmba_mountains)

area_df <- hkh_gmba_subset|>
  select(MapName,Area)|>
  st_drop_geometry()|>
  rename(GMBA_mountain_range = MapName)

#----------------------------------------------------------#
# species richness per 1000 km area-----
#----------------------------------------------------------#

# Count species richness per mountain range
species_richness <- full_assessment_hkh_mammals %>%
  separate_rows(GMBA_mountain_range, sep = ", ") %>%
  group_by(GMBA_mountain_range) %>%
  summarise(richness = n_distinct(sciname))|>
  left_join(area_df, by = "GMBA_mountain_range")|>
  mutate(log_richness = log1p(richness),
         log_area=log1p(Area),
         richness_per_1000km2 = richness / (Area / 1000),
         richness_residuals = residuals(lm(log_richness ~ log_area))
         )


#----------------------------------------------------------#
# map it ----
#----------------------------------------------------------#

# Join species richness to spatial data
hkh_gmba_subset_join <- hkh_gmba_subset %>%
  left_join(species_richness, by = c("MapName" = "GMBA_mountain_range"))

# Calculate centroids or representative points for labeling
mountain_labels <- hkh_gmba_subset_join %>%
  group_by(MapName) %>%
  mutate(label_point = st_point_on_surface(geometry)) %>%
  st_as_sf() %>%
  mutate(x = st_coordinates(label_point)[,1],
         y = st_coordinates(label_point)[,2])



# Override specific label positions
mountain_labels <- mountain_labels %>%
  mutate(
    x = case_when(
      MapName == "Balochistan Ranges" ~ 66,  # <- Adjust as needed
      MapName == "Himalaya" ~ 83.0,           # <- Adjust as needed
      TRUE ~ x
    ),
    y = case_when(
      MapName == "Balochistan Ranges" ~ 25,  # <- Adjust as needed
      MapName == "Himalaya" ~ 26.3,           # <- Adjust as needed
      MapName == "Hindu Kush"~38.5,
      MapName == "Karakoram" ~ 37,
      TRUE ~ y
    )
  )

# Load world map and filter for Asia
world <- ne_countries(scale = "medium", returnclass = "sf")
asia <- world %>% filter(region_un == "Asia")

# Define bounding box for HKH
hkh_bbox <- st_bbox(c(xmin = 62, xmax = 105, ymin = 17, ymax = 40), crs = st_crs(asia))

# Plot
species_map <- ggplot() +
  geom_sf(data = asia, fill = "gray95", color = "gray80", size = 0.3) +
  geom_sf(data = hkh_gmba_subset_join, aes(fill = richness_per_1000km2), color = "black", size = 0.01, alpha = 0.6) +
  geom_sf(data = hkh_boundaries, fill = NA, color = "darkblue", size = 1) +
  geom_text(
    data = mountain_labels,
    aes(x = x, y = y, label = MapName),
    size = 4,              # Controls text size (try 4–6 for maps)
    color = "black",
    fontface = "italic"      # Use "italic", "bold", etc.
  )+
  scale_fill_viridis_c(option = "inferno", name = "Mammal richness per 1000km2", direction = -1) +
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
    plot.title = element_text(hjust = 0.5, size = 14, face = "italic"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

plot(species_map)

# Save plot
ggsave(filename = paste0(plot_output_path, "hkh_gmba_species_richness_map_area.jpg"), 
       plot = species_map, width = 10, height = 6, dpi = 300)

# View plot
species_map
