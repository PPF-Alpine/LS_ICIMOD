#                         
#
#----------------------------------------------------------#

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#

library(readxl)
library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(rvest)
library(rredlist)
library(lwgeom)


#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
hkh_boundary <- sf::st_read(paste(data_storage_path,"HKH_Boundary/HKH_Boundary.shp", sep = "/"))

all_mammals <- readxl::read_xlsx(paste0(data_storage_path,"HKH_list/alpine_mammal_database_HKH.xlsx"))

hkh_gmba_mountains <- c("Himalaya",
                        "Hindu Kush",
                        "Balochistan Ranges",
                        "Karakoram",
                        "Tibetan Plateau")# these are the mountain ranges included in HKH (completeley)

hkh_mammals<- all_mammals|>
  filter(Mountain_range%in%hkh_gmba_mountains)|>
  select(sciname,order,family,Mountain_range,habitat,min_elevation,max_elevation,max_elevation_mountain_range,expert_validated,mean_treeline,)


#----------------------------------------------------------#
# restructure data ----
#----------------------------------------------------------#

# Add a presence column
hkh_mammals <- hkh_mammals %>%
  mutate(presence = "yes")

# Pivot data to wide format
hkh_mammals_wide <- hkh_mammals %>%
  pivot_wider(
    names_from = Mountain_range,
    values_from = c(presence, min_elevation, max_elevation,
                    max_elevation_mountain_range, expert_validated, mean_treeline),
    names_sep = "_"
  )

# Replace NA in presence columns with "no"
presence_cols <- grep("^presence_", names(hkh_mammals_wide), value = TRUE)
hkh_mammals_wide[presence_cols] <- lapply(hkh_mammals_wide[presence_cols], function(x) ifelse(is.na(x), "no", x))

# Optional: rename columns to suffix "_GMBA"
names(hkh_mammals_wide) <- str_replace_all(names(hkh_mammals_wide), "^presence_", "")
names(hkh_mammals_wide) <- str_replace_all(names(hkh_mammals_wide), "^", function(x) {
  # add GMBA only to mountain range-based fields
  ifelse(grepl(paste(hkh_gmba_mountains, collapse = "|"), x), paste0(x, "_GMBA"), x)
})


# save the data 
library(writexl)
write_xlsx(hkh_mammals_wide,paste(data_storage_path,"hkh_mammals_LS.xlsx"))




