
# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#

library(here)
library(rredlist)
library(rlist)
library(purrr)


iucn_key <-"6U8o8k6YEKh8TaGDeNonEezQQyu9vMYLUCyd"

#----------------------------------------------------------#
# Get IUCN description and classification
#----------------------------------------------------------#

habitats_classification <- rl_habitats(key=iucn_key)

habitats_classification_codes <- as.data.frame(habitats_classification$habitats)|>
  mutate(category="habitat")


conservation_classification <- rl_actions(key=iucn_key)

conservation_classification_codes <- as.data.frame(conservation_classification$conservation_actions)|>
  mutate(category="conservation_action")


classification_IUCN_habitat_cons_actions<-bind_rows(conservation_classification_codes,habitats_classification_codes)


#----------------------------------------------------------#
# write as character column and save as csv
#----------------------------------------------------------#
classification_IUCN_habitat_cons_actions$description_en <- as.character(classification_IUCN_habitat_cons_actions$description$en)

classification_IUCN_habitat_cons_actions$description <- NULL


write_csv(classification_IUCN_habitat_cons_actions, paste0(data_storage_path, "RL_assessments/IUCN_classification_codes_habitat_cons_actions.csv"))
