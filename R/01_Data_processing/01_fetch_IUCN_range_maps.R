#----------------------------------------------------------#                       Config file
#                 
#
#           Get IUCN ranges for HKH species 
#                         
#
#----------------------------------------------------------#


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
# Set up -----
#----------------------------------------------------------#
# you might need an IUCN API for this script
# iucn_token <-"2tjwLmiFvhHqTJP2xi1UTofVMkrBp8kE7e3B"

# download terrestrial mammal data from IUCN
# https://www.iucnredlist.org/resources/spatial-data-download


#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
range_polygon_terr_mammals <- sf::st_read(paste(data_storage_path,"HKH_list/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp", sep = "/"))


HKH_mammals <- readxl::read_xlsx(paste0(data_storage_path,"HKH_list/ICIMOD_HKH_mammals.xlsx"))


# have to merge and restructure the datasets for him and hk

#----------------------------------------------------------#
# restructure data ----
#----------------------------------------------------------#
cols_static <- c("sciname", "order", "family", "Mountain_system", 
                 "habitat", "expert_validated")

# Split and rename only relevant columns
mammals_himalaya <- HKH_mammals %>%
  filter(Mountain_range == "Himalaya") %>%
  rename_with(~ paste0(., "_Himalaya"), .cols = !all_of(cols_static))

mammals_hindukush <- HKH_mammals %>%
  filter(Mountain_range == "Hindu Kush") %>%
  rename_with(~ paste0(., "_Hindukush"), .cols = !all_of(cols_static))


merged_mammals <- full_join(mammals_himalaya, mammals_hindukush, by = c("Mountain_system","sciname","order","family","habitat"))

## needs some more cleaning.. 

#----------------------------------------------------------#
# merge the distribution range polygons ----
#----------------------------------------------------------#

hkh_ranges <- merged_mammals|>
  left_join(
    range_polygon_terr_mammals|>
      select(sci_name, id_no, compiler, citation, yrcompiled, category, SHAPE_Area, geometry),
    by = c("sciname" = "sci_name")
  )

# validate shapes 
hkh_ranges <- validate_shapes_individually(hkh_ranges)


# some species have more than one range polygon.. union 
hkh_ranges_sum <- hkh_ranges %>%
  group_by(sciname) %>%
  summarise(
    SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE),
    geometry = st_union(st_combine(geometry))
  ) %>%
  ungroup()


