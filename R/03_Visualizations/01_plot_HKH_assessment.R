
# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)

# load RL and HKH list
HKH_RL <- read.csv(paste0(data_storage_path,"RL_assessments/HKH_RL_assessments_23052025.csv"))

HKH_list <- read.csv(paste0(data_storage_path,"HKH_list/HKH_mammals_LS_23052025.csv"))|>
  select(-X)

global_RL <- read.csv(paste0(data_storage_path,"RL_assessments/global_assessment_HKH_mammals_full.csv"))

unique(HKH_RL$countries_iso)

#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

HKH_RL_select <- HKH_RL|>
  select(sciname,status_code,countries_iso,year_assessed)|>
  rename(status_code_national=status_code)|>
  rename(year_assessed_national =year_assessed)

global_RL_select <- global_RL|>
  select(sciname,red_list_code,year_published)|>
  rename(status_code_global=red_list_code)|>
  rename(year_assessed_global =year_published)

HKH_list_select <- HKH_list|>
  select(sciname,order,family,average_min_elevation,average_max_elevation)

plot_data <- global_RL_select|>
  left_join(HKH_RL_select,by="sciname")|>
  filter(str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore")|>
  group_by(sciname, countries_iso) |>
  filter(year_assessed_national == max(year_assessed_national, na.rm = TRUE)) |>
  ungroup()


#----------------------------------------------------------#
# plot assesments -----
#----------------------------------------------------------#

#  countries for national assessments
national_long <- plot_data|>
  select(sciname, status_code_national, countries_iso)|>
  rename(status_code = status_code_national, region = countries_iso)

# Prepare global data
global_long <- plot_data|>
  select(sciname, status_code_global)|>
  distinct()|>
  rename(status_code = status_code_global)|>
  mutate(region = "Global")

# Combine both
combined_data <- bind_rows(national_long, global_long)

# count number of species per region and status
agg_data <- combined_data|>
  group_by(region, status_code)|>
  summarise(species_count = n_distinct(sciname), .groups = "drop")

# Plot
ggplot(agg_data, aes(x = region, y = species_count, fill = status_code)) +
  geom_bar(stat = "identity") +
  labs(
    title = "HKH RL assessment",
    x = "Region",
    y = "Number of Species",
    fill = "Red List Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
