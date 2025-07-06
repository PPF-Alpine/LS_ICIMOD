# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
unique(full_assessment_hkh_mammals$countries_iso)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/assessment_hkh_mammals_25062025_LS.csv"))|>
  filter(
  str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore"
)

#----------------------------------------------------------#
# overview threathened species -----
#----------------------------------------------------------#

# how many species are threathened
full_assessment_hkh_mammals |>
  filter(
    status_summary_national == "threatened"|status_summary_national =="data deficient"
  ) |>
  distinct(sciname)|>
  count()
#346

# how many species are threatened in more than one country
full_assessment_hkh_mammals |>
  filter(status_summary_national == "threatened"|status_summary_national =="data deficient") |>
  group_by(sciname) |>
  filter(n_distinct(countries_iso) > 1) |>
  summarise() |>
  nrow()
#161

# number of threatened species
# number for each status code national for each country
# as overview table
n_total_threatened <- full_assessment_hkh_mammals |>
  filter(status_summary_national == "threatened") |>
  distinct(sciname) |>
  nrow()

# count by status_code_national per country
threat_status_by_country <- full_assessment_hkh_mammals |>
  filter(status_code_national != "LC") |>
  group_by(countries_iso, status_code_national) |>
  summarise(n_species = n_distinct(sciname), .groups = "drop")

# threat by order 
order_threat<-full_assessment_hkh_mammals |>
  filter(
    status_summary_national == "threatened"|status_summary_national =="data deficient"
  ) |>
  group_by(order,status_code_national)|>
  count()

# threat by order 
order_threat<-full_assessment_hkh_mammals |>
  filter(
    status_summary_national == "threatened"|status_summary_national =="data deficient"
  ) |>
  group_by(order)|>
  count()

# across ecosystems 
#----------------------------------------------------------#
# high concern species that are listed across several countries and have decreasing pop trend -----
#----------------------------------------------------------#
# identify which species are:
# threathened across more than one country (status code national summary  == threathened)
# AND are also threathened globally (status code global summary)
# AND have population trend == Decreasing

# identify species that are nationally threatened in more than one country
national_threat_multi_country <- full_assessment_hkh_mammals |>
  filter(
    status_summary_national == "threatened"
  ) |>
  group_by(sciname) |>
  filter(n_distinct(countries_iso) > 1) |>
  ungroup()

sciname_multi_country <- full_assessment_hkh_mammals |>
  filter(
    status_summary_national == "threatened"
  ) |>
  group_by(sciname) |>
  filter(n_distinct(countries_iso) > 1) |>
  ungroup() |>
  pull(sciname)|>
  unique()

# filter to only include those species that:
# - are nationally threatened in multiple countries (as identified above)
# - AND globally threatened
# - AND have a decreasing population trend
result <- full_assessment_hkh_mammals |>
  filter(
    sciname %in% sciname_multi_country,
    status_summary_global == "threatened",
    pop_trend_global == "Decreasing"
  )

length(unique(result$sciname))

# 40 species are listed as threathened across several countries and also globally and there population trend global is decreasing

species_summary <- result |>
  group_by(sciname) |>
  summarise(
    common_name = str_trim(str_split(common_names_iucn[1], ";")[[1]][1]),
    countries_threatened = paste(unique(countries_iso), collapse = "; "),
    status_code_national = paste(status_code_national, collapse = "; "),
    year_assessed_national = paste(year_assessed_national, collapse = "; "),
    status_code_global = first(status_code_global),
    pop_trend_global = first(pop_trend_global),
    year_assessed_global = first(year_assessed_global),
  )

write.csv(species_summary,paste0(data_storage_path,"RL_assessments/high_concern_species_nrl_poptrend.csv"))

#----------------------------------------------------------#
  # high concern species that are listed across several countries 
#----------------------------------------------------------#

result_multi_country<-full_assessment_hkh_mammals |>
  filter(status_summary_national == "threatened") |>
  group_by(sciname) |>
  filter(n_distinct(countries_iso) > 1) 


# Summarise into same format
species_summary_multi_country <- result_multi_country |>
  group_by(sciname) |>
  summarise(
    common_name = str_trim(str_split(common_names_iucn[1], ";")[[1]][1]),
    countries_threatened = paste(unique(countries_iso), collapse = "; "),
    status_code_national = paste(status_code_national, collapse = "; "),
    year_assessed_national = paste(year_assessed_national, collapse = "; "),
    wildlife_protection_india_2022 = first(wildlife_protection_india_2022),
    status_code_global = first(status_code_global),
    pop_trend_global = first(pop_trend_global),
    year_assessed_global = first(year_assessed_global),
    .groups = "drop"
  )|>
  rename(wildlife_protection_india=wildlife_protection_india_2022)

# Export
write.csv(species_summary_multi_country, paste0(data_storage_path, "RL_assessments/multi_country_threatened_species.csv"))
