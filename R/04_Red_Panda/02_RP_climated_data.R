#----------------------------------------------------------#
# chelsa climate data -
#----------------------------------------------------------#

plot(hfi_rp)

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(terra)

# Load configuration file
source(here::here("R/00_Config_file.R"))

rp_distr <- sf::st_read(paste0(data_storage_path, "/red_panda_IUCN/data_0.shp"))

#----------------------------------------------------------#
#         load chelsa data and mountain range
#----------------------------------------------------------#

annual_temp <- terra::rast("C:/Users/lotta/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_Conservation_assessment_mountains/Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio1_1981-2010_V.2.1.tif")

annual_prec <- terra::rast("C:/Users/lotta/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_Conservation_assessment_mountains/Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio12_1981-2010_V.2.1.tif")


# this is averaged data for ssp85 2040-2070
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))
annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio12_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))




# temp
temp_mountain_crop <- crop(annual_temp, rp_distr)
temp_mountain <- mask(temp_mountain_crop, rp_distr)
x11()
plot(temp_mountain)

#prec
prec_mountain_crop <- crop(annual_prec, rp_distr)
prec_mountain <- mask(prec_mountain_crop, rp_distr)


# temp future
temp_mountain_crop_future <- crop(annual_temp_ssp85, rp_distr)
temp_mountain_future <- mask(temp_mountain_crop_future, rp_distr)


#prec future
prec_mountain_crop_future <- crop(annual_prec_ssp85, mountain_range)
prec_mountain_future <- mask(prec_mountain_crop_future, mountain_range)

#----------------------------------------------------------#
#       stack temp and prec rasters current 
#----------------------------------------------------------#
# Stack rasters 
climate_stack <- c(temp_mountain, prec_mountain)
names(climate_stack) <- c("temperature", "precipitation")

climate_df <- as.data.frame(climate_stack, na.rm = TRUE)

# estimate current climate range within RP habitat

# min and max would be to extreme.. 
# ‼️needs a sensitivity analysis which quantiles to be used
temp_range <- quantile(climate_df$temperature, probs = c(0.05, 0.95), na.rm = TRUE)
prec_range <- quantile(climate_df$precipitation, probs = c(0.05, 0.95), na.rm = TRUE)

current_alpine_area <- (temp_mountain >= temp_range[1]) & (temp_mountain <= temp_range[2]) &
  (prec_mountain >= prec_range[1]) & (prec_mountain <= prec_range[2])

#----------------------------------------------------------------------#
#   stack temp and prec rasters future 
#----------------------------------------------------------------------#
#
climate_stack_future <- c(temp_mountain_future, prec_mountain_future)
names(climate_stack_future) <- c("temperature", "precipitation")


# for each pixel find if these climates are still within the current alpine climatic range
future_alpine_area <- (temp_mountain_future >= temp_range[1]) & (temp_mountain_future <= temp_range[2]) &
  (prec_mountain_future >= prec_range[1]) & (prec_mountain_future <= prec_range[2])

# true shows where alpine area will persist and false where not 
x11()
plot(current_alpine_area)

#----------------------------------------------------------------------#
#  calculate how much clim space is lost
#----------------------------------------------------------------------#

#  count  suitable pixels
current_pixels <- global(current_alpine_area, "sum", na.rm = TRUE)[1,1] # terrra::global is safer than sum
future_pixels <- global(future_alpine_area, "sum", na.rm = TRUE)[1,1]


# % retained
percent_retained <- (future_pixels / current_pixels) * 100
# Percent lost
percent_lost <- 100 - percent_retained


print(paste0("Percent of climate space lost: ", round(percent_lost, 1), "%"))

#----------------------------------------------------------------------#
#  plot the alpine climatic space lost 
#----------------------------------------------------------------------#

# Logical to numeric: TRUE -> 1, FALSE -> 0
future_alpine_area_numeric <- classify(future_alpine_area, rcl = matrix(c(0, 1, 1, 2), ncol = 2, byrow = TRUE)) - 1

future_alpine_df <- as.data.frame(future_alpine_area_numeric, xy = TRUE)

# After converting raster to dataframe
future_alpine_df$cells <- factor(future_alpine_df$`CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1`,
                                 levels = c(1, 0),
                                 labels = c("retained", "lost"))



ggplot(future_alpine_df, aes(x = x, y = y, fill = cells)) +
  geom_tile() +
  scale_fill_manual(values = c("retained" = "yellow", "lost" = "darkred")) +
  coord_equal() +
  theme_minimal() +
  labs(title = paste(mountain_range_name, ": future alpine climate space retained/lost (RCP8.5 2040-2070)"),
       fill = "cells",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


