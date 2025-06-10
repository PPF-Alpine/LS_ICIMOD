
# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# read assesment -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(patchwork)
library(sf)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(RColorBrewer)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_08062025.csv"))


# read shps
hkh_gmba <- sf::st_read(paste(data_storage_path,"HKH_Boundary/HKH_GMBA_clip.shp", sep = "/"))

hkh_gmba_mountains <- c("Himalaya",
                        "Hindu Kush",
                        "Balochistan Ranges",
                        "Karakoram",
                        "Tibetan Plateau")

hkh_gmba_subset <- hkh_gmba|>
  filter(MapName%in%hkh_gmba_mountains)

hkh_boundaries <- sf::st_read(paste(data_storage_path,"HKH_Boundary/HKH_Boundary.shp", sep = "/"))


# Count species richness per mountain range
species_richness <- df %>%
  separate_rows(GMBA_mountain_range, sep = ", ") %>%
  group_by(GMBA_mountain_range) %>%
  summarise(richness = n_distinct(sciname))|>
  mutate(log_richness = log1p(richness))

# Join species richness to spatial data
hkh_gmba_subset_join <- hkh_gmba_subset %>%
  left_join(species_richness, by = c("MapName" = "GMBA_mountain_range"))




# Load world map and filter for Asia
world <- ne_countries(scale = "medium", returnclass = "sf")
asia <- world %>% filter(region_un == "Asia")

# Define bounding box for HKH
hkh_bbox <- st_bbox(c(xmin = 62, xmax = 105, ymin = 17, ymax = 40), crs = st_crs(asia))

# Plot
species_map <- ggplot() +
  geom_sf(data = asia, fill = "gray95", color = "gray80", size = 0.3) +
  geom_sf(data = hkh_gmba_subset_join, aes(fill = log_richness), color = "black", size = 0.1, alpha = 0.8) +
  geom_sf(data = hkh_boundaries, fill = NA, color = "darkblue", size = 1) +
  scale_fill_viridis_c(option = "inferno", name = "Log Species Richness",direction = -1) +
  coord_sf(xlim = c(hkh_bbox["xmin"], hkh_bbox["xmax"]), ylim = c(hkh_bbox["ymin"], hkh_bbox["ymax"])) +
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
plot(species_map)

# Save plot
plot_output_path <- paste0(data_storage_path, "Visualizations/")
ggsave(filename = paste0(plot_output_path, "hkh_gmba_species_richness_map.jpg"), 
       plot = species_map, width = 10, height = 6, dpi = 300)

# View plot
species_map
