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

# the description dataframe for the classification codes by IUCN
classification_code_description <- read_delim(
  "C:/Users/lotta/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_Conservation_assessment_mountains/Outputs/IUCN_assessment_lists/classification_code_description.csv",
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ","),
  show_col_types = FALSE
)

classification_code_description$code<-as.factor(classification_code_description$code)


classification_codes_ecosystem <- read.csv(paste0(data_storage_path,"RL_assessments/IUCN_classification_codes_habitat_cons_actions.csv"))|>
  filter(category=="habitat")|>
  rename(description_ecosystem = description_en)|>
  rename(code_ecosystem = code)|>
  filter(!code_ecosystem %in% c(9, 10, 12, 13, 15, 17))


#----------------------------------------------------------#
#        prep data
#----------------------------------------------------------#

threats_stresses_species_broad <- threats_stresses_species|>
  mutate(threats_broad = threats_code |>
           str_replace_all("_", ".") |>                   # Replace underscores with dots
           str_split(";\\s*") |>                          # Split into list by ";"
           lapply(function(x) str_extract(x, "^\\d+")) |> # Extract first number from each part
           lapply(unique) |>                              # Keep unique top-level threats
           sapply(paste, collapse = "; ")                 # Collapse back into a single string
  ) |>
  mutate(stresses_broad = stresses_code |>
           str_replace_all("_", ".") |>                            # Normalize to dots
           str_split(";\\s*") |>                                   # Split by ;
           lapply(function(x) str_extract(x, "^\\d+\\.\\d+")) |>   # Extract first two levels (e.g., 2.3)
           lapply(unique) |>                                       # Keep unique values
           sapply(paste, collapse = "; ")                          # Collapse back into one string
  )|>
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


threats_long <- threats_stresses_species_broad |>
  separate_rows(threats_broad, sep = ";\\s*") |>
  separate_rows(ecosystem_broad, sep = ";\\s*") |>  # separate ecosystems
  filter(!is.na(threats_code) & threats_code != "") |>
  distinct() |>
  group_by(sciname, threats_broad, ecosystem_broad) |>
  slice(1) |>
  ungroup()




#----------------------------------------------------------#
# count threats
#----------------------------------------------------------#

threat_counts <- threats_long |>
  group_by(ecosystem_broad, threats_broad) |>
  count(name = "n") |>
  left_join(classification_code_description, by = c("threats_broad" = "code")) |>
  mutate(
    description_threats = as.character(description),
    fill_order = factor(description_threats, levels = unique(description_threats))
  ) |>
  left_join(classification_codes_ecosystem, by = c("ecosystem_broad" = "code_ecosystem"))  # add ecosystem descriptions

# Ensure consistent factor levels (same order in every facet)
threat_counts <- threat_counts |>
  mutate(description_threats = factor(description_threats, levels = unique(description_threats)))

threat_counts <- threat_counts |>
  filter(!is.na(description_ecosystem))

plot_count <- ggplot(threat_counts, aes(
  x = description_threats,
  y = n,
  fill = n
)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_c(option = "C", direction = -1, guide = "none") +
  labs(
    x = NULL,
    y = "Threat count"
  ) +
  facet_wrap(~ description_ecosystem, nrow = 3, scales = "fixed") +
  coord_flip() +  # 🔄 Horizontal bars
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    strip.text = element_text(face = "bold",size=14),
    panel.spacing = unit(1, "lines")
  )


x11()
plot(plot_count)



plot_output_path <- paste0(data_storage_path, "Visualizations/")

ggsave(filename = paste0(plot_output_path, "IUCN_threats_HKH_mammals_in_ecosystems.jpg"),
       plot = plot_count, width = 25, height = 20, dpi = 300)




