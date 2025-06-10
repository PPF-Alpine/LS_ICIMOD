
# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# read assesment -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(patchwork)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/additional_tables/full_assessment_hkh_mammals_02062025.csv"))

#----------------------------------------------------------#
# add GMBA mountain ranges -----
#----------------------------------------------------------#
df <- full_assessment_hkh_mammals|>
  rowwise() |>
  mutate(GMBA_mountain_range = paste(
    c(
      ifelse(!is.na(min_elevation_himalaya) | !is.na(max_elevation_himalaya), "Himalaya", NA),
      ifelse(!is.na(min_elevation_karakoram) | !is.na(max_elevation_karakoram), "Karakoram", NA),
      ifelse(!is.na(min_elevation_tibetan_plateau) | !is.na(max_elevation_tibetan_plateau), "Tibetan Plateau", NA),
      ifelse(!is.na(min_elevation_balochistan_ranges) | !is.na(max_elevation_balochistan_ranges), "Balochistan Ranges", NA),
      ifelse(!is.na(min_elevation_hindu_kush) | !is.na(max_elevation_hindu_kush), "Hindu Kush", NA)
    ) %>% na.omit(), collapse = ", "
  )) |>
  mutate(GMBA_mountain_range = ifelse(GMBA_mountain_range == "", "Himalaya", GMBA_mountain_range)) |>
  ungroup()

write.csv(df,paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_08062025.csv"))

# NA for no elev
df <- full_assessment_hkh_mammals|>
  rowwise() |>
  mutate(GMBA_mountain_range = paste(
    c(
      ifelse(!is.na(min_elevation_himalaya) | !is.na(max_elevation_himalaya), "Himalaya", NA),
      ifelse(!is.na(min_elevation_karakoram) | !is.na(max_elevation_karakoram), "Karakoram", NA),
      ifelse(!is.na(min_elevation_tibetan_plateau) | !is.na(max_elevation_tibetan_plateau), "Tibetan Plateau", NA),
      ifelse(!is.na(min_elevation_balochistan_ranges) | !is.na(max_elevation_balochistan_ranges), "Balochistan Ranges", NA),
      ifelse(!is.na(min_elevation_hindu_kush) | !is.na(max_elevation_hindu_kush), "Hindu Kush", NA)
    ) %>% na.omit(), collapse = ", "
  )) |>
  #mutate(GMBA_mountain_range = ifelse(GMBA_mountain_range == "", "Himalaya", GMBA_mountain_range)) |>
  ungroup()

#----------------------------------------------------------#
# taxonomic summaries --
#----------------------------------------------------------#
# Summary: Number of unique species per order and family
taxonomic_summary <- df %>%
  group_by(order, family) %>%
  summarise(unique_species = n_distinct(sciname)) %>%
  arrange(desc(unique_species))

# Overview: Total unique species per order
order_summary <- df %>%
  group_by(order) %>%
  summarise(unique_species = n_distinct(sciname)) %>%
  arrange(desc(unique_species))

# Overview: Total unique species per family
family_summary <- df %>%
  group_by(family) %>%
  summarise(unique_species = n_distinct(sciname)) %>%
  arrange(desc(unique_species))

#----------------------------------------------------------#
# fill NAs -----
#----------------------------------------------------------#

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_10062025.csv"))|>
  select(-X.2,-X.1,-X,-X.3)|>
  mutate(
    sciname = str_trim(sciname),
    sciname = case_when(
      sciname == "Aonyx cinerea" ~ "Aonyx cinereus",
      sciname == "Areilulus circumdatus" ~ "Arielulus circumdatus",
      sciname == "Crocidura attenuate" ~ "Crocidura attenuata",
      sciname == "Crocidura horsfieldi"~"Crocidura horsfieldii",
      sciname == "Mus cookie"~"Mus cookii",
      sciname == "Ochotona roylei"~"Ochotona roylii",
      sciname == "Rousettus leschenaulti"~"Rousettus leschenaultii",
      sciname == "Soriculus nigriscens"~"Soriculus nigrescens",
      sciname == "Tamiops macclellandii"~ "Tamiops mcclellandii",
      TRUE ~ sciname
    )
  )
colnames(full_assessment_hkh_mammals)

# List of columns to fill
cols_to_fill <- c("order","family","common_name","common_names",
  "habitat_HMW", "min_elevation_himalaya","max_elevation_himalaya","min_elevation_tibetan_plateau","max_elevation_tibetan_plateau","min_elevation_balochistan_ranges","max_elevation_balochistan_ranges","min_elevation_hindu_kush","max_elevation_hindu_kush","min_elevation_karakoram", "max_elevation_karakoram","average_min_elevation","average_max_elevation","wildlife_protection_india")

# Function to fill down and up within groups
fill_group_na <- function(df, cols) {
  df %>%
    group_by(sciname) %>%
    arrange(sciname) %>%
    fill(all_of(cols), .direction = "downup") %>%
    ungroup()
}

# Apply the fill function
assessment_updated <- fill_group_na(full_assessment_hkh_mammals, cols_to_fill)

assessment_updated<- assessment_updated|>
  mutate(
    common_names_iucn = case_when(
      !is.na(common_names) & !is.na(common_name) ~ str_c(common_names, common_name, sep = ","),
      !is.na(common_names) ~ common_names,
      !is.na(common_name) ~ common_name,
      TRUE ~ NA_character_
    )
  )|>
  select(sciname,order,family,common_names_iucn,status_code_national,status_summary_national,countries_iso,year_assessed_national,publication_national,publication_citation_national,wildlife_protection_india,status_code_global,status_summary_global,year_assessed_global,iucn_criteria_global,citation_iucn_global,threats_code,threats_description,threats_global,stresses_code,stresses_description,pop_trend_global,range_global,country_code_global,habitats_iucn,habitat_HMW,GMBA_mountain_range,average_min_elevation,average_max_elevation,min_elevation_himalaya,max_elevation_himalaya,min_elevation_tibetan_plateau,max_elevation_tibetan_plateau,min_elevation_balochistan_ranges,max_elevation_balochistan_ranges,min_elevation_hindu_kush,max_elevation_hindu_kush,min_elevation_karakoram,max_elevation_karakoram)|>
  rename(wildlife_protection_india_2022 = wildlife_protection_india)


#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

write.csv(assessment_updated,paste0(data_storage_path,"RL_assessments/assessment_hkh_mammals_10062025_LS.csv"))



