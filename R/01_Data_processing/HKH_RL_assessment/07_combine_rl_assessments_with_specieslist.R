# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)

# load RL and HKH list
HKH_RL <- read.csv(paste0(data_storage_path,"RL_assessments/HKH_RL_assessments_23052025.csv"))

HKH_list <- read.csv(paste0(data_storage_path,"HKH_list/HKH_mammals_LS_27052025.csv"))|>
  select(-X)|>
  rename(habitat_HMW = habitat)

global_RL <- read.csv(paste0(data_storage_path,"RL_assessments/global_assessment_HKH_mammals_full_26052025.csv"))

length(unique(global_RL_select$sciname))
#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

HKH_RL_select <- HKH_RL|>
  select(sciname,common_names,
         status_code,status_summary,countries_iso,year_assessed,publication,publication_citation)|>
  rename(status_code_national=status_code)|>
  rename(year_assessed_national =year_assessed)|>
  rename(status_summary_national=status_summary)|>
  rename(publication_national=publication)|>
  rename(publication_citation_national=publication_citation)|>
  group_by(sciname, countries_iso) |> # filter only for the most recent assessment
  filter(year_assessed_national == max(year_assessed_national, na.rm = TRUE)) |>
  ungroup()


global_RL_select <- global_RL|>
  select(-order_name,-family_name,-red_list_version,-X)|>
  rename(status_code_global=red_list_code)|>
  rename(year_assessed_global =year_published)|>
  rename(range_global =range)|>
  rename(threats_global =threats_summary)|>
  rename(iucn_criteria_global =criteria)|>
  rename(citation_iucn_global =citation)|>
  group_by(sciname) |> # filter only for the most recent assessment
  slice_max(year_assessed_global, with_ties = FALSE) |>
  ungroup()

#----------------------------------------------------------#
# join to FULL HKH list -----
#----------------------------------------------------------#

# this needs cleaning --> species that are not all in HKH e.g., now the full china list is joined --> they need to go out

full_assessment_hkh_mammals <- HKH_list|>
  full_join(HKH_RL_select,by="sciname")|>
  left_join(global_RL_select,by="sciname")

#nas_nepal <- full_assessment_hkh_mammals|>
 # filter((is.na(family) & countries_iso == "Nepal"))|>
  #select(sciname,order,family,min_elevation_himalaya,max_elevation_himalaya)

full_assessment_hkh_mammals <- full_assessment_hkh_mammals|>
filter(!(is.na(family) & countries_iso != "Nepal"))|>
  mutate(
    status_summary_global = case_when( # add a summary column for IUCN status
      status_code_global %in% c("CR", "EN", "VU", "NT", "LE", "RE") ~ "threatened",
      status_code_global == "NE" ~ "not evaluated",
      status_code_global == "LC" ~ "not threatened",
      status_code_global == "DD" ~ "data deficient",
      TRUE ~ "NA"  
    )
  )

#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

write.csv(full_assessment_hkh_mammals,paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_27052025.csv"))


write.csv(nas_nepal,paste0(data_storage_path,"RL_assessments/nas_nepal.csv"))


