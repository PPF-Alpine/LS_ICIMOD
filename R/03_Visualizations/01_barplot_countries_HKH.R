# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_23052025.csv"))

full_ass_work_data <- full_assessment_hkh_mammals|>
  filter(str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore")|>
  select(sciname,status_code_national,status_summary_national,status_code_global,countries_iso)

#----------------------------------------------------------#
# plot assesments numbers per category-----
#----------------------------------------------------------#

status_levels <- c("DD", "NE", "NA","RE", "CR", "EN", "VU", "NT", "LC")

national_plot <- full_ass_work_data |>
  select(sciname, countries_iso, status_code_national) |>
  rename(region = countries_iso, status_code = status_code_national)

global_plot <- full_ass_work_data |>
  select(sciname, status_code_global) |>
  distinct() |>
  mutate(region = "Global") |>
  rename(status_code = status_code_global)

plot_data <- bind_rows(national_plot, global_plot) |>
  mutate(status_code = factor(status_code, levels = status_levels))

region_order <- plot_data |>
  group_by(region) |>
  summarise(total_species = n_distinct(sciname), .groups = "drop") |>
  arrange(desc(total_species)) |>
  pull(region) |>
  {\(x) c(setdiff(x, "Global"), "Global")}()

plot_data <- plot_data |>
  mutate(region = factor(region, levels = region_order))

agg_plot <- plot_data |>
  group_by(region, status_code) |>
  summarise(species_count = n_distinct(sciname), .groups = "drop")


ggplot(agg_plot, aes(x = region, y = species_count, fill = status_code)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "inferno", na.value = "lightgrey", direction = 1) +
  labs(
    title = "Number of threathened mammals in HKH",
    x = "Region",
    y = "Number of Species",
    fill = "Status Code"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#----------------------------------------------------------#
# plot assesments relative categories -----
#----------------------------------------------------------#
# Calculate proportions per region
rel_plot <- plot_data |>
  group_by(region, status_code) |>
  summarise(species_count = n_distinct(sciname), .groups = "drop") |>
  group_by(region) |>
  mutate(prop = species_count / sum(species_count)) |>
  ungroup()

# Plot relative proportions
ggplot(rel_plot, aes(x = region, y = prop, fill = status_code)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "inferno", na.value = "lightgrey", direction = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of threathened mammals in HKH per country",
    x = "Region",
    y = "Percentage of Species",
    fill = "Status Code"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

