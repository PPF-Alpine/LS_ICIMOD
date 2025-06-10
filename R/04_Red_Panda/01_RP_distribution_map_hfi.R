# Load configuration file
source(here::here("R/00_Config_file.R"))
library(leaflet)
library(htmlwidgets)
library(terra)
#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

rp_distr <- sf::st_read(paste0(data_storage_path, "/red_panda_IUCN/data_0.shp"))

# plot range of RP 
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Map") %>%
  addPolygons(data = rp_distr,
              color = "darkblue",
              weight = 2,
              fillOpacity = 0.1,
              group = "RP") 

# show how future climate conditions change in its range

#----------------------------------------------------------#
# Load human footprint index (HFI) 
#----------------------------------------------------------#

hfi<- terra::rast("C:/Users/lotta/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_Conservation_assessment_mountains/Datasets/human_footprint/HFI/hfi_mountains.tif")

#----------------------------------------------------------#
# crop and mask HFI to RP range  -
#----------------------------------------------------------#

hfi_cropped <- crop(hfi, rp_distr)

hfi_rp <- mask(hfi_cropped, rp_distr, touches=TRUE)

plot(hfi_rp)

# Convert to data frame for plotting
hfi_df <- as.data.frame(hfi_rp, xy = TRUE, na.rm = TRUE)

# Rename column if needed
names(hfi_df)[3] <- "HFI"

# Group by longitude bins (or unique longitudes, if resolution is manageable)
hfi_long_summary <- hfi_df %>%
  group_by(x) %>%         # x is longitude
  summarise(mean_HFI = mean(HFI, na.rm = TRUE))

# Plot
ggplot(hfi_long_summary, aes(x = x, y = mean_HFI)) +
  geom_line(color = "darkred") +
  geom_vline(xintercept = c(87.8, 88.6), linetype = "dashed", color = "blue") +
  annotate("text", x = 88.2, y = max(hfi_long_summary$mean_HFI, na.rm = TRUE), 
           label = "Ilam to Sikkim", vjust = -0.5, color = "blue", size = 3.5)+
  labs(
    x = "Longitude",
    y = "Mean Human Footprint Index",
    title = "HFI across latitudinal gradient within Red Panda habitat"
  ) +
  theme_minimal()

writeRaster(hfi_rp, paste0(data_storage_path, "red_panda_IUCN/human_footprint_rp.tif"), overwrite=TRUE)


