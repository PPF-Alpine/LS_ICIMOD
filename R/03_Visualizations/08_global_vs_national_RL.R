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


# Step 1: Get total unique species per global status
label_data <- threats_stresses_species %>%
  distinct(sciname, status_code_global) %>%  # <-- Ensure uniqueness
  count(status_code_global, name = "total_species")

# Step 2: Prepare full summary table (still by national/global combo)
summary_table <- threats_stresses_species %>%
  count(status_code_global, status_code_national)

# Step 3: Join totals and create facet labels
summary_table <- summary_table %>%
  left_join(label_data, by = "status_code_global") %>%
  mutate(facet_label = paste0(status_code_global, " (", total_species, ")"))

# Filter out category "R"
summary_table <- summary_table %>%
  filter(status_code_national != "R")

# Set factor levels in desired order
summary_table$status_code_national <- factor(
  summary_table$status_code_national,
  levels = c("DD","NE","RE","CR","EN", "VU","NT","LC"),
  ordered = TRUE
)

plot <- ggplot(summary_table, aes(x = "", y = n, fill = status_code_national)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ facet_label, scales = "free") +
  scale_fill_viridis_d(option = "inferno",na.value = "lightgrey", name = "National Status") +
  labs(
    x = NULL,
    y = NULL,
    title = "National status within each global IUCN status"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 20),
    plot.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    legend.key.size = unit(1.5, "cm"),
    legend.spacing.y = unit(0.5, "cm")
  )

  
plot




plot_output_path <- paste0(data_storage_path, "Visualizations/")

ggsave(filename = paste0(plot_output_path, "national_vs_global_status.jpg"),
       plot = plot, width = 25, height = 20, dpi = 300)


library(dplyr)

# Step 1: Get unique global status per species
global_status <- threats_stresses_species %>%
  select(sciname, status_code_global) %>%
  distinct()

# Step 2: Compare national vs. global status per species-country
status_comp <- threats_stresses_species %>%
  select(sciname, status_code_national, countries_iso) %>%
  left_join(global_status, by = "sciname") %>%
  mutate(
    match_type = case_when(
      is.na(status_code_global) | is.na(status_code_national) ~ "Missing Data",
      status_code_global == status_code_national ~ "Match",
      TRUE ~ "Mismatch"
    )
  )

# Step 3: Summarise match results per species
species_match_summary <- status_comp %>%
  group_by(sciname, status_code_global) %>%
  summarise(
    total_national_assessments = n(),
    matches = sum(match_type == "Match"),
    mismatches = sum(match_type == "Mismatch"),
    missing = sum(match_type == "Missing Data"),
    .groups = "drop"
  )

# Step 4: Count how many species are fully matched, mismatched, or partial
species_match_summary %>%
  mutate(overall_status = case_when(
    mismatches == 0 & matches > 0 ~ "Fully Matched",
    mismatches > 0 & matches > 0 ~ "Partially Matched",
    mismatches > 0 & matches == 0 ~ "Fully Mismatched",
    TRUE ~ "Only Missing Data"
  )) %>%
  count(overall_status)

# Step 5: Mismatch rates by GLOBAL status
match_by_global <- status_comp %>%
  group_by(status_code_global) %>%
  summarise(
    total = n(),
    matches = sum(match_type == "Match"),
    mismatches = sum(match_type == "Mismatch"),
    match_rate = matches / total,
    mismatch_rate = mismatches / total,
    .groups = "drop"
  ) %>%
  arrange(desc(mismatch_rate))

# Step 6: Mismatch rates by NATIONAL status
match_by_national <- status_comp %>%
  group_by(status_code_national) %>%
  summarise(
    total = n(),
    matches = sum(match_type == "Match"),
    mismatches = sum(match_type == "Mismatch"),
    match_rate = matches / total,
    mismatch_rate = mismatches / total,
    .groups = "drop"
  ) %>%
  arrange(desc(mismatch_rate))

# Step: Filter and summarize national status for globally LC species
status_comp %>%
  filter(status_code_global == "NT" & !is.na(status_code_national)) %>%
  count(status_code_national, sort = TRUE)

