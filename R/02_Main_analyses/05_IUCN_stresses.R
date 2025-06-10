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

threats_stresses_species <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_08062025.csv"))

# the description dataframe for the classification codes by IUCN
classification_code_description <- read_delim(
  "C:/Users/lotta/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_Conservation_assessment_mountains/Outputs/IUCN_assessment_lists/classification_code_description.csv",
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ","),
  show_col_types = FALSE
)

classification_code_description$code<-as.factor(classification_code_description$code)

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
  mutate(status_code_global = coalesce(status_code_global, "not assessed"))


stresses_long <- threats_stresses_species_broad |>
  separate_rows(stresses_broad, sep = ";\\s*") |>
  filter(!is.na(stresses_code) & stresses_code != "") |>
  distinct() |>
  group_by(sciname,stresses_broad) |> # for unique species and unique threats
  slice(1) |>
  ungroup()


#----------------------------------------------------------#
# calculate proportion and count of threats per elev band
#----------------------------------------------------------#

stress_counts <- stresses_long |>
  group_by(stresses_broad) |>
  count(name = "n") |>
  left_join(classification_code_description, by = c("stresses_broad" = "code")) |>
  arrange(desc(n)) |>
  mutate(
    description = as.character(description),  # make sure it's character first
    fill_order = factor(description, levels = unique(description))  # ordered for fill
  )

plot_count <- ggplot(stress_counts, aes(
  x = reorder(description, -n),
  y = n,
  fill = n  # use actual count for fill
)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_c(option = "C", direction = -1,guide = "none") +  # continuous color scale
  labs(
    x = NULL,
    y = "Stress count",
    fill = "Stress count"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

plot(plot_count)



ggsave(filename = paste0(plot_output_path, "IUCN_stresses_HKH_mammals.jpg"),
       plot = plot_count, width = 4, height = 3, dpi = 300)






