# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# read assesment -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(patchwork)
library(dplyr)
library(stringr)
library(purrr)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/assessment_hkh_mammals_25062025_LS.csv"))

#----------------------------------------------------------#
# filter country codes -----
#----------------------------------------------------------#

# Define HKH country codes
hkh_countries <- c("IN", "NP", "CN", "MM", "PK", "BD", "AF", "BT")

# Assuming country_code_global is a string with codes separated by ;
full_assessment_country <- full_assessment_hkh_mammals %>%
  mutate(hkh_country_code = str_split(country_code_global, ";") %>%
           lapply(function(codes) {
             codes <- str_trim(codes)
             hkh <- codes[codes %in% hkh_countries]
             str_c(hkh, collapse = ";")
           }) %>%
           unlist())

#----------------------------------------------------------#
# get country codes from national assessment -----
#----------------------------------------------------------#

## add if needed the country codes from national assessments

country_name_to_iso <- c(
  "India" = "IN", "Nepal" = "NP", "China" = "CN", "Myanmar" = "MM",
  "Pakistan" = "PK", "Bangladesh" = "BD", "Afghanistan" = "AF", "Bhutan" = "BT"
)

#
national_codes <- full_assessment_hkh_mammals %>%
  select(sciname, countries_iso) %>%
  filter(countries_iso != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore") %>%
  mutate(
    country_iso_national = country_name_to_iso[countries_iso]
  ) %>%
  filter(!is.na(country_iso_national))  


national_codes_wide <- national_codes %>%
  distinct(sciname, country_iso_national) %>%  # Remove duplicates
  group_by(sciname) %>%
  summarise(
    national_codes_combined = paste(sort(unique(country_iso_national)), collapse = ";"),
    .groups = "drop"
  )

# join with national codes 
full_assessment_country<-full_assessment_country|>
  left_join(national_codes_wide,by="sciname")

# merge into one country code column
merged_df <- full_assessment_country %>%
  rowwise() %>%
  mutate(
    # Wrap vectors in list()
    hkh_vec = list(if (!is.na(hkh_country_code)) str_split(hkh_country_code, ";")[[1]] else character(0)),
    national_vec = list(if (!is.na(national_codes_combined)) str_split(national_codes_combined, ";")[[1]] else character(0)),
    
    # Combine and deduplicate
    merged_hkh = paste(sort(unique(c(hkh_vec, national_vec))), collapse = ";")
  ) %>%
  select(-hkh_vec, -national_vec) %>%
  ungroup()

#----------------------------------------------------------#
# get country codes from national assessment -----
#----------------------------------------------------------#
country_summary <- merged_df %>%
  separate_rows(merged_hkh, sep = ";") %>%         # Split countries
  mutate(merged_hkh = str_trim(merged_hkh)) %>%  # Clean whitespace
  group_by(merged_hkh) %>%
  summarise(species_richness = n_distinct(sciname), .groups = "drop")

#----------------------------------------------------------#
# plot the data  -----
#----------------------------------------------------------#

library(sf)
library(rnaturalearth)
library(dplyr)

# Load global country polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for Asian countries if needed (or just HKH ones)
hkh_countries <- c("IN", "NP", "CN", "MM", "PK", "BD", "AF", "BT")

asia <- world %>% filter(iso_a2 %in% hkh_countries)|>
  select(iso_a2,geometry)
plot(asia)

hkh_boundaries <- sf::st_read(paste(data_storage_path,"HKH_Boundary/HKH_Boundary.shp", sep = "/"))

# Clip them to the HKH boundary
hkh_countries_clipped <- st_intersection(asia, hkh_boundaries)
plot(hkh_countries_clipped)

# join and select relevant data 
country_summary <- country_summary %>%
  mutate(log_richness = log1p(species_richness))  # log1p = log(1 + x)

hkh_countries_clipped_sum <- hkh_countries_clipped %>%
  left_join(country_summary, by = c("iso_a2" = "merged_hkh"))|>
  select(iso_a2,log_richness,species_richness,geometry)


# Define bounding box for HKH
hkh_bbox <- st_bbox(c(xmin = 62, xmax = 105, ymin = 17, ymax = 40), crs = st_crs(asia))

species_map <- ggplot() +
  geom_sf(data = asia, fill = "gray95", color = "gray80", size = 0.3) +
  geom_sf(data = hkh_countries_clipped_sum, aes(fill = log_richness), color = "black", size = 0.3) +  # <- use this one
  geom_sf(data = hkh_boundaries, fill = NA, color = "darkblue", size = 1) +
  scale_fill_viridis_c(option = "inferno", name = "Log Species Richness", direction = -1) +
  coord_sf(xlim = c(hkh_bbox["xmin"], hkh_bbox["xmax"]),
           ylim = c(hkh_bbox["ymin"], hkh_bbox["ymax"])) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

plot(species_map)

# Save plot
plot_output_path <- paste0(data_storage_path, "Visualizations/")
ggsave(filename = paste0(plot_output_path, "hkh_country_species_richness_map.svg"), 
       plot = species_map, width = 10, height = 6, dpi = 300)

# View plot
species_map

#----------------------------------------------------------#
# plot the data  -----
#----------------------------------------------------------#
library(sf)
library(dplyr)

# Make sure you're in a projected CRS in meters — needed for area calculation
# We'll use EPSG:32644 (UTM zone 44N) or any other relevant UTM zone for HKH
hkh_countries_clipped_sum_proj <- hkh_countries_clipped_sum %>%
  st_transform(crs = 32644)  # You can adjust the UTM zone if needed

# Calculate area in km2 and richness per 1000 km2
hkh_countries_area <- hkh_countries_clipped_sum_proj %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,  # Convert m² to km²
    richness_density = species_richness / (area_km2 / 1000),
    log_density = log1p(richness_density)
  )

species_map <- ggplot() +
  geom_sf(data = asia, fill = "gray95", color = "gray80", size = 0.3) +
  geom_sf(data = hkh_countries_area, aes(fill = log_density), color = "black", size = 0.3) +
  geom_sf(data = hkh_boundaries, fill = NA, color = "darkblue", size = 1) +
  scale_fill_viridis_c(option = "inferno", name = "Log Richness / 1000 km²", direction = -1) +
  coord_sf(xlim = c(hkh_bbox["xmin"], hkh_bbox["xmax"]),
           ylim = c(hkh_bbox["ymin"], hkh_bbox["ymax"])) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

plot(species_map)

# Save plot
plot_output_path <- paste0(data_storage_path, "Visualizations/")
ggsave(filename = paste0(plot_output_path, "hkh_country_area_species_richness_map.svg"), 
       plot = species_map, width = 10, height = 6, dpi = 300)

# View plot
species_map

