#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(rredlist)
library(stringr)
library(scales)
library(purrr)
library(patchwork)
#install.packages("Matrix")
library(lme4)
# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#        read in the threats
#----------------------------------------------------------#
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/assessment_hkh_mammals_25062025_LS.csv"))


library(dplyr)

# Step 1: Create 500 m elevation bands
elevation_bands <- ecosystem_long |>
  mutate(
    elev_min_band = floor(average_min_elevation / 500) * 500,
    elev_max_band = floor(average_max_elevation / 500) * 500
  ) |>
  rowwise() |>
  mutate(elevation_band = list(seq(elev_min_band, elev_max_band, by = 500))) |>
  unnest(elevation_band) |>
  ungroup()

# Step 2: Total species per elevation band
total_species_elev <- elevation_bands |>
  group_by(elevation_band) |>
  summarise(total_species = n_distinct(sciname), .groups = "drop")

# Step 3: Threatened species per elevation band
threatened_species_elev <- elevation_bands |>
  filter(status_code_national %in% c("CR", "EN", "VU", "NT", "DD")) |>
  group_by(elevation_band) |>
  summarise(threatened_species = n_distinct(sciname), .groups = "drop")

# Step 4: Calculate proportions
elevation_threat_summary <- total_species_elev |>
  left_join(threatened_species_elev, by = "elevation_band") |>
  mutate(
    threatened_species = replace_na(threatened_species, 0),
    proportion_threatened = threatened_species / total_species
  )
