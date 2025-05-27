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

test <- rl_species_latest(genus = "Vulpes", species = "vulpes", key = iucn_key)

sciname <- test$taxon$scientific_name
order <- test$taxon$order_name
family <- test$taxon$family_name
upper_limit <- as.numeric(test$supplementary_info$upper_elevation_limit)
lower_limit <- as.numeric(test$supplementary_info$lower_elevation_limit)
  
# load RL and HKH list
df <- read.csv(paste0(data_storage_path,"RL_assessments/nas_nepal.csv"))



# get dataframe with genus and species
spp <- df %>%
  select(sciname) %>%
  # Separate into genus and species by space
  separate(sciname, into = c("genus", "species"), sep = " ", remove = FALSE)|>
  distinct()


#sp_assessment_list <- rl_species_latest(genus = "Ovis", species = "ammon",key=iucn_key)

#----------------------------------------------------------#
#   get a dataframe with info of interest for all species 
#----------------------------------------------------------#

spp <- spp|>
  select(genus,species)

# Apply across all species
iucn_elevations <- pmap_dfr(spp[, c("genus", "species")], get_and_clean_iucn_elevations)



write.csv(iucn_elevations,paste0(data_storage_path,"RL_assessments/iucn_elevations.csv"))




