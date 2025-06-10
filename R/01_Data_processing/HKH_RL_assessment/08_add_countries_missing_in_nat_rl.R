
# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# read assesment -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(patchwork)

# added india and pakistan RL her
# load RL and HKH list
#full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_27052025.csv"))


#----------------------------------------------------------#
# restructure pakistan and indian assessment  -----
#----------------------------------------------------------#


## for pakistan and india there are not the most up to data lists --> complement assessment with this data
pakistan_rl <- read.csv(paste0(data_storage_path,"RL_assessments/national/pakistan_national_rl.csv"))|>
  mutate(countries_iso = "Pakistan")|>
  mutate(year_assessed_national = "2003")|>
  mutate(publication_national = "Status and Red List of Pakistan’s Mammals, Pakistan Mammal Conservation Assessment & Management Plan Workshop ")|>
  mutate(publication_citation_national = "Kashif M. Sheikh, Sanjay Molur et al., 2003, Status and Red List of Pakistan's Mammals")|>
  rename(status_code_national = status_code)|>
  mutate(
    status_summary_national = case_when( # add a summary column for IUCN status
      status_code_national %in% c("CR", "EN", "VU", "NT", "LE", "RE") ~ "threatened",
      status_code_national == "NE" ~ "not evaluated",
      status_code_national == "LC" ~ "not threatened",
      status_code_national == "DD" ~ "data deficient",
      TRUE ~ "NA"  
    )
  )|>
  select(sciname,
         status_code_national,
         status_summary_national,countries_iso,year_assessed_national,publication_national,publication_citation_national)

#Ex=Extinct, E=Endangered, V=Vulnerable, I=Indeterminate, K=Insufficiently known & T=Threatened

# column category
# Ex = RE
# E = EN
# I = DD
# K = DD
# V = VU
# T = CR

india_rl <- readxl::read_xlsx(paste0(data_storage_path,"RL_assessments/national/mr_files_RL/forestry_mammals_india.xlsx"))|>
  mutate(countries_iso = "India")|>
  mutate(year_assessed_national = "2017")|>
  mutate(publication_national = "A checklist of mammals of India, Ministry of forest and environment")|>
  mutate(publication_citation_national = "A checklist of mammals of India, M. Kamalakannan and C. Venkatraman, Zoological Survey of India, 2017")|>
  mutate(status_code_national = recode(category,
                           "Ex" = "RE",
                           "E"  = "EN",
                           "I"  = "DD",
                           "K"  = "DD",
                           "V"  = "VU",
                           "T"  = "CR"))|>
  mutate(
    status_summary_national = case_when( # add a summary column for IUCN status
      status_code_national %in% c("CR", "EN", "VU", "NT", "LE", "RE") ~ "threatened",
      status_code_national == "NE" ~ "not evaluated",
      status_code_national == "LC" ~ "not threatened",
      status_code_national == "DD" ~ "data deficient",
      TRUE ~ "NA"  
    )
  )|>
  select(sciname,
         status_code_national,
         status_summary_national,countries_iso,year_assessed_national,publication_national,publication_citation_national)


# Combine India and Pakistan data
new_data <- bind_rows(india_rl, pakistan_rl) %>%
  mutate(year_assessed_national = as.numeric(year_assessed_national))

# Filter only those rows where sciname exists in the full assessment
new_data_filtered <- new_data %>%
  filter(sciname %in% full_assessment_hkh_mammals$sciname)

# Remove existing rows that will be updated
assessment_cleaned <- full_assessment_hkh_mammals %>%
  anti_join(new_data_filtered, by = c("sciname", "countries_iso"))

# Combine cleaned + filtered updates (no new scinames added)
assessment_updated <- bind_rows(assessment_cleaned, new_data_filtered)


# check that joint is correct 

# Find species-country combinations that were updated
#updated_rows <- inner_join(full_assessment_hkh_mammals, new_data, by = c("sciname", "countries_iso"))

# Optional: show just the species names
#updated_species <- updated_rows %>%
 # select(sciname, countries_iso)


colnames(assessment_updated)

# fill rows for the global assessments for the newly added species

# List of columns to fill
cols_to_fill <- c(
  "order", "family", "year_assessed_global", "iucn_criteria_global",
  "citation_iucn_global", "status_code_global", "threats_code", "threats_description",
  "stresses_code", "stresses_description", "common_name", "pop_trend_global",
  "range_global", "habitats_iucn", "threats_global", "country_code_global", "status_summary_global"
)

# Function to fill down and up within groups
fill_group_na <- function(df, cols) {
  df %>%
    group_by(sciname) %>%
    arrange(sciname) %>%
    fill(all_of(cols), .direction = "downup") %>%
    ungroup()
}

# Apply the fill function
assessment_updated <- fill_group_na(assessment_updated, cols_to_fill)


#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

write.csv(assessment_updated,paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_02062025.csv"))


# added india wildlife protection schedules here
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_08062025.csv"))

india_wp <- readxl::read_xlsx(paste0(data_storage_path,"RL_assessments/national/mr_files_RL/India_wildlife_schedules_cleaned.xlsx"))|>
  select(scientific_name,schedule)|>
  rename(sciname=scientific_name)|>
  rename(wildlife_protection_india = schedule)


assessment_updated <- full_assessment_hkh_mammals|>
  left_join(india_wp,by="sciname")


write.csv(assessment_updated,paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_10062025.csv"))

