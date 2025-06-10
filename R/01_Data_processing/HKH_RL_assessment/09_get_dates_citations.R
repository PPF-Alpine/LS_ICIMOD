# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_23052025.csv"))

dates_citations <- full_assessment_hkh_mammals |>
  select(
    countries_iso,
    year_assessed_national,
    publication_national,
    publication_citation_national
  ) |>
  arrange(countries_iso, year_assessed_national)|>
  distinct()|>
  drop_na()


# there are some species that have not been assessed under the most recent assessment in 2011
species_nepal_2005 <- full_assessment_hkh_mammals |>
  filter(
    countries_iso == "Nepal",
    year_assessed_national == 2005
  ) |>
  distinct() 

#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

write.csv(dates_citations,paste0(data_storage_path,"RL_assessments/dates_citations_26052025.csv"))

