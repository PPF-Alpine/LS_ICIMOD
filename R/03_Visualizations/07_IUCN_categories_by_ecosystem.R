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
  filter(!code_ecosystem %in% c(9, 10, 12, 13, 15, 17))


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




#----------------------------------------------------------#
# count threats
#----------------------------------------------------------#

threat_counts <- ecosystem_long |>
  group_by(ecosystem_broad, status_code_national) |>
  count(name = "n") |>
  left_join(classification_codes_ecosystem, by = c("ecosystem_broad" = "code_ecosystem"))  # add ecosystem descriptions

# Filter out category "R"
threat_counts <- threat_counts %>%
  filter(status_code_national != "R")

# Set factor levels in desired order
threat_counts$status_code_national <- factor(
  threat_counts$status_code_national,
  levels = c("NA", "DD", "LC", "NT", "VU", "EN", "CR", "NE", "RE"),
  ordered = TRUE
)


plot_count <- ggplot(threat_counts, aes(
  x = status_code_national,
  y = n,
  fill = n
)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_c(option = "C", direction = -1, guide = "none") +
  labs(
    x = NULL,
    y = "IUCN Category count"
  ) +
  facet_wrap(~ description_ecosystem, nrow = 3, scales = "fixed") +
  coord_flip() +  # 🔄 Horizontal bars
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    strip.text = element_text(face = "bold",size=18),
    panel.spacing = unit(1, "lines")
  )


x11()
plot(plot_count)

#NA, DD, LD, NT, VU,EN,CR,NE,RE

plot_output_path <- paste0(data_storage_path, "Visualizations/")

ggsave(filename = paste0(plot_output_path, "IUCN_threats_HKH_mammals_in_ecosystems.jpg"),
       plot = plot_count, width = 25, height = 20, dpi = 300)




