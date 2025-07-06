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

threats_stresses_species <- read.csv(paste0(data_storage_path,"RL_assessments/assessment_hkh_mammals_25062025_LS.csv"))

classification_codes_ecosystem <- read.csv(paste0(data_storage_path,"RL_assessments/IUCN_classification_codes_habitat_cons_actions.csv"))|>
  filter(category=="habitat")|>
  rename(description_ecosystem = description_en)|>
  rename(code_ecosystem = code)|>
  filter(!code_ecosystem %in% c(2,9, 10, 12, 13, 15, 17))


#----------------------------------------------------------#
#        prep data
#----------------------------------------------------------#

threats_stresses_species_broad <- threats_stresses_species|>
  mutate(ecosystem_broad = ecosystem_code_iucn |>
           str_replace_all("_", ".") |>                   # Replace underscores with dots
           str_split(";\\s*") |>                          # Split into list by ";"
           lapply(function(x) str_extract(x, "^\\d+")) |> # Extract first number from each part
           lapply(unique) |>                              # Keep unique top-level threats
           sapply(paste, collapse = "; ")                 # Collapse back into a single string
  )|>
  mutate(cons_action_broad = conservation_action_code_iucn |>
           str_replace_all("_", ".") |>                   # Replace underscores with dots
           str_split(";\\s*") |>                          # Split into list by ";"
           lapply(function(x) str_extract(x, "^\\d+")) |> # Extract first number from each part
           lapply(unique) |>                              # Keep unique top-level threats
           sapply(paste, collapse = "; ")                 # Collapse back into a single string
  )|>
  mutate(status_code_global = coalesce(status_code_global, "not assessed"))

ecosystem_long <- threats_stresses_species_broad |>
  separate_rows(ecosystem_broad, sep = ";\\s*") |>  # separate ecosystems
  #filter(!is.na(ecosystem_code_iucn) & ecosystem_code_iucn != "") |>
  distinct() |>
  group_by(sciname, ecosystem_broad,status_code_national) |>
  slice(1) |>
  ungroup()

# count threats across ecosystem 
ecosy_threat <- ecosystem_long |>
  filter(
    status_summary_national == "threatened"|status_summary_national =="data deficient"
  ) |>
  group_by(ecosystem_broad,status_code_national)|>
  count()|>
  arrange()|>
  left_join(classification_codes_ecosystem, by = c("ecosystem_broad" = "code_ecosystem"))


########### proportion
library(dplyr)

# Step 1: Total species per ecosystem
total_species <- ecosystem_long |>
  group_by(ecosystem_broad) |>
  summarise(total_species = n_distinct(sciname))  # Replace species_id with the appropriate species identifier

# Step 2: Threatened or data deficient species per ecosystem
threatened_species <- ecosystem_long |>
  filter(status_summary_national %in% c("threatened", "data deficient")) |>
  group_by(ecosystem_broad) |>
  summarise(threatened_species = n_distinct(sciname))  # same species_id

# Step 3: Join and calculate proportion
ecosystem_proportion <- total_species |>
  left_join(threatened_species, by = "ecosystem_broad") |>
  mutate(
    threatened_species = replace_na(threatened_species, 0),
    proportion_threatened = threatened_species / total_species
  ) |>
  left_join(classification_codes_ecosystem, by = c("ecosystem_broad" = "code_ecosystem"))|>
  filter(!is.na(description_ecosystem))


# for each status code 

ecosystem_status_counts <- ecosystem_long |>
  filter(status_code_national %in% c("NT", "VU", "EN", "DD", "LC", "RE", "CR", "NE")) |>
  group_by(ecosystem_broad, status_code_national) |>
  summarise(n_species = n_distinct(sciname), .groups = "drop")

# Step 2: Join classification information for ecosystems
ecosystem_status_counts <- ecosystem_status_counts |>
  left_join(classification_codes_ecosystem, by = c("ecosystem_broad" = "code_ecosystem")) |>
  filter(!is.na(description_ecosystem))

ecosystem_status_prop <- ecosystem_status_counts |>
  left_join(total_species, by = c("ecosystem_broad")) |>
  mutate(proportion = n_species / total_species)

# Step 4: Join with classification codes
ecosystem_status_final <- ecosystem_status_prop |>
  left_join(classification_codes_ecosystem, by = c("ecosystem_broad" = "code_ecosystem","category","description_ecosystem")) 


# Set factor order for status codes
status_levels <- c("DD", "NE", "NA", "RE", "CR", "EN", "VU", "NT", "LC")

ecosystem_status_final <- ecosystem_status_final |>
  mutate(
    status_code_national = factor(status_code_national, levels = status_levels),
    description_ecosystem = fct_reorder(description_ecosystem, proportion, .fun = sum, .desc = TRUE)
  ) |>
  group_by(ecosystem_broad) |>
  mutate(proportion_norm = proportion / sum(proportion)) |>
  ungroup()|>
  filter(description_ecosystem!="Unknown")


# Plot
ggplot(ecosystem_status_final, aes(x = proportion_norm, y = description_ecosystem, fill = status_code_national)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(
    option = "inferno",
    na.value = "lightgrey",
    direction = 1
  ) +
  labs(
    x = "Proportion of Species",
    y = NULL,
    fill = "Status"
  ) +
  theme_minimal()



