#                         
#
#----------------------------------------------------------#

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#

library(here)
library(rredlist)
library(rlist)
library(purrr)
# Load configuration file
source(here::here("R/00_Config_file.R"))

iucn_key <-"6U8o8k6YEKh8TaGDeNonEezQQyu9vMYLUCyd"

#----------------------------------------------------------#
# Load data-----
#----------------------------------------------------------#

HKH_mammals <- readxl::read_xlsx(paste0(data_storage_path,"HKH_list/HKH_mammals_LS_cleaned.xlsx"))|>
  janitor::clean_names()

nat_RL <- readxl::read_xlsx(paste0(data_storage_path,"RL_assessments/national_assessment_nepal_mammals.xlsx"))|>
  janitor::clean_names()|>
  rename(sciname=scientific_name)

colnames(HKH_mammals)
#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

HKH_rl <- HKH_mammals|>
  left_join(nat_RL,by="sciname")|>
  rename(status_nepal = status)


# Assuming your data frame is named df
HKH_rl_mut <- HKH_rl %>%
  mutate(
    average_min_elevation = round(rowMeans(select(., 
                                                  min_elevation_himalaya, 
                                                  min_elevation_tibetan_plateau, 
                                                  min_elevation_balochistan_ranges, 
                                                  min_elevation_hindu_kush, 
                                                  min_elevation_karakoram
    ), na.rm = TRUE)),
    
    average_max_elevation = round(rowMeans(select(., 
                                                  max_elevation_himalaya, 
                                                  max_elevation_tibetan_plateau, 
                                                  max_elevation_balochistan_ranges, 
                                                  max_elevation_hindu_kush, 
                                                  max_elevation_karakoram
    ), na.rm = TRUE))
  )


#----------------------------------------------------------#
# plot -----
#----------------------------------------------------------#


# Assuming your data frame is called `df`
# Step 1: Calculate mid-elevation
HKH_rl_mut <- HKH_rl_mut %>%
  mutate(
    mid_elevation = round((average_min_elevation + average_max_elevation) / 2)
  )

breaks <- seq(0, max(HKH_rl_mut$mid_elevation, na.rm = TRUE) + 250, by = 250)

# Create readable labels like "0–250", "250–500"
labels <- paste0(breaks[-length(breaks)], "–", breaks[-1])

HKH_rl_mut <- HKH_rl_mut %>%
  mutate(
    elevation_bin = cut(mid_elevation, breaks = breaks, labels = labels, include.lowest = TRUE),
    elevation_bin = as.character(elevation_bin)  # This prevents scientific notation in plotting
  )

HKH_rl_mut$elevation_bin <- factor(HKH_rl_mut$elevation_bin, levels = labels)


# Step 3: Count species per elevation bin and threat status
elev_counts <- HKH_rl_mut %>%
  group_by(elevation_bin, status_nepal) %>%
  summarise(species_count = n(), .groups = "drop")

# Step 4: Plot
ggplot(elev_counts, aes(x = elevation_bin, y = species_count, fill = status_nepal)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  theme_minimal(base_size = 14) +
  labs(
    title = "IUCN status along elevational gradient",
    x = "Mid Elevation (binned, meters)",
    y = "Number of Species",
    fill = "IUCN Status"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


