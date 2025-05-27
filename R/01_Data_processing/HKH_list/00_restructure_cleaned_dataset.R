#                         
#
#----------------------------------------------------------#

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#

library(here)
library(rredlist)
library(rlist)
library(purrr)
library(rgbif)
library(tibble)

# Load configuration file
source(here::here("R/00_Config_file.R"))

#iucn_key <-"6U8o8k6YEKh8TaGDeNonEezQQyu9vMYLUCyd"

#----------------------------------------------------------#
# Load data-----
#----------------------------------------------------------#

HKH_mammals <- readxl::read_xlsx(paste0(data_storage_path,"HKH_list/HKH_mammals_LS_cleaned.xlsx"))|>
  janitor::clean_names()

length(unique(HKH_mammals$sciname))
HKH_mammals_unique <- HKH_mammals[duplicated(HKH_mammals$sciname), ]

# Assuming your data frame is named df
HKH_mammals_restr <- HKH_mammals %>%
  mutate(
    average_min_elevation = round(rowMeans(select(., 
                                                  min_elevation_himalaya, 
                                                  min_elevation_tibetan_plateau, 
                                                  min_elevation_balochistan_ranges, 
                                                  min_elevation_hindu_kush, 
                                                  min_elevation_karakoram
    ), na.rm = TRUE)),
    
    average_max_elevation = round(rowMeans(select(., 
                                                  max_elevation_himalaya, 
                                                  max_elevation_tibetan_plateau, 
                                                  max_elevation_balochistan_ranges, 
                                                  max_elevation_hindu_kush, 
                                                  max_elevation_karakoram
    ), na.rm = TRUE))
  )|>
  select(-mean_treeline_himalaya,
         -mean_treeline_tibetan_plateau,
         -mean_treeline_balochistan_ranges,
         -mean_treeline_hindu_kush,
         -mean_treeline_karakoram)


#----------------------------------------------------------#
# get the higher taxonomy for the species--
#----------------------------------------------------------#

# Extract the vector of scientific names
#scinames <- HKH_mammals_restr|>
 # pull(sciname)

# fetch gbif taxonomy
#taxonomy_df <- map_dfr(scinames, get_gbif_taxonomy)

write.csv(HKH_mammals_restr,paste0(data_storage_path,"HKH_list/HKH_mammals_LS_27052025.csv"))

