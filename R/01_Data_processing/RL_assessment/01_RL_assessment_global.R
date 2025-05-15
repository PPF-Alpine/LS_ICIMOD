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
# Load configuration file
source(here::here("R/00_Config_file.R"))

iucn_key <-"6U8o8k6YEKh8TaGDeNonEezQQyu9vMYLUCyd"

#----------------------------------------------------------#
# Load data-----
#----------------------------------------------------------#

df <- readxl::read_xlsx(paste0(data_storage_path,"HKH_list/HKH_mammals_LS_cleaned.xlsx"))|>
  janitor::clean_names()
#----------------------------------------------------------#
#        get the species names in format
#----------------------------------------------------------#

# get dataframe with genus and species
spp <- df %>%
  select(sciname) %>%
  # Separate into genus and species by space
  separate(sciname, into = c("genus", "species"), sep = " ", remove = FALSE)

#----------------------------------------------------------#
#   get a dataframe with info of interest for all species 
#----------------------------------------------------------#

# Apply across all species
assessment_threats_complete <- pmap_dfr(spp[, c("genus", "species")], get_and_clean_iucn)

sp_assessment <- rl_species_latest(genus = "Ovis", species = "ammon",key=iucn_key)


write.csv(assessment_threats_complete,paste0(data_storage_path,"RL_assessments/global_assessment_HKH_mammals.csv"))
