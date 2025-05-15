#                         
#
#----------------------------------------------------------#

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#

library(readxl)
library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(rvest)
library(rredlist)
library(janitor)

df <- readxl::read_xlsx(paste0(data_storage_path,"HKH_list/HKH_mammals_LS_cleaned.xlsx"))|>
  janitor::clean_names()

# plot 

# Reshape to long format
library(tidyverse)

df_long <- df %>%
  select(sciname = 1, 
         min_elevation_himalaya, max_elevation_himalaya,
         min_elevation_tibetan_plateau, max_elevation_tibetan_plateau,
         min_elevation_hindu_kush, max_elevation_hindu_kush,
         min_elevation_karakoram, max_elevation_karakoram,
         min_elevation_balochistan_ranges, max_elevation_balochistan_ranges) %>%
  pivot_longer(
    cols = -sciname,
    names_to = c(".value", "range"),
    names_pattern = "(min|max)_elevation_(.*)"
  ) %>%
  drop_na(min, max) %>%
  mutate(mid_elevation = (min + max) / 2)

# Plot
ggplot(df_long) +
  geom_linerange(aes(x = mid_elevation, ymin = min, ymax = max),
                 alpha = 0.6, linewidth = 0.3, color = "darkorange") +
  geom_point(aes(x = mid_elevation, y = mid_elevation),
             size = 0.7, alpha = 0.9, color = "darkorange") +
  facet_wrap(~range, scales = "free_y", ncol = 4) +
  scale_y_continuous(name = "Elevation (m)") +
  scale_x_continuous(name = "Midpoint Elevation") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12))


ggplot(df_long, aes(x = range, y = mid_elevation)) +
  geom_violin(fill = "lightblue", alpha = 0.6, color = NA, scale = "width") +
  geom_jitter(width = 0.2, size = 0.7, alpha = 0.8, color = "black") +
  scale_y_continuous(name = "Midpoint Elevation (m)") +
  scale_x_discrete(name = "Mountain Range") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank()) +
  labs(title = "mammals midpoint elevations across HKH")

species_count <- df_long %>%
  group_by(range) %>%
  summarise(n_species = n_distinct(sciname)) %>%
  arrange(desc(n_species))

print(species_count)
