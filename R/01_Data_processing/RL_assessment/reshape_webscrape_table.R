# Load configuration file
source(here::here("R/00_Config_file.R"))


library(tidyverse)
library(rgbif)


RL_national <- read.csv(paste0(data_storage_path,"RL_assessments/national_assessment_webscraping_output.csv"))|>
  janitor::clean_names()


RL_national <- RL_national|>
  mutate(
    genus = word(species_or_taxon, 1),
    species = word(species_or_taxon, 2),
    sciname = paste(genus, species),
    status_code = str_extract(status, "[A-Z]{2}(?=[^A-Za-z]*$)"))|>
  select(sciname,
         genus,species,
         species_or_taxon,
         common_names,
         year_assessed,
         countries_iso,
         locality,
         publication,
         criteria_system,
         status,status_code,publication_citation)|>
  mutate(
    status_summary = case_when(
      status_code %in% c("CR", "EN", "VU", "NT", "LE", "RE") ~ "threatened",
      status_code == "NE" ~ "not evaluated",
      status_code == "LC" ~ "not threatened",
      status_code == "DD" ~ "data deficient",
      TRUE ~ "NA"  # Catch-all for unexpected codes
    )
  )
  

#----------------------------------------------------------#
# get the higher taxonomy for the species--
#----------------------------------------------------------#
# Extract the vector of scientific names
scinames <- RL_national|>
  pull(sciname)

# fetch gbif taxonomy
taxonomy_df <- map_dfr(scinames, get_gbif_taxonomy)

#----------------------------------------------------------#
# filter for HKH countries -
#----------------------------------------------------------#

HKH_countries <- c("Nepal", 
                   "Afghanistan", 
                   "Bhutan", 
                   "Pakistan", 
                    "Myanmar", 
                   "India", 
                   "Bangladesh", 
                   "China")

RL_national_filter <- RL_national |>
  filter(str_detect(countries_iso, str_c(HKH_countries, collapse = "|")))


