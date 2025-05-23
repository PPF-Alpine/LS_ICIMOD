# Load configuration file
source(here::here("R/00_Config_file.R"))

library(circlize)
library(igraph)
library(ggraph)
library(ggplot2)

#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_23052025.csv"))

full_ass_work_data <- full_assessment_hkh_mammals|>
  filter(str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore")|>
  select(sciname,status_code_national,status_summary_national,status_code_global,countries_iso)

#----------------------------------------------------------#
# shared mammals threathened -----
#----------------------------------------------------------#

shared_species <- full_ass_work_data |>
  filter(status_summary_national %in% c("threatened", "data deficient")) |>
  separate_rows(countries_iso, sep = ",\\s*") |>
  distinct(sciname, countries_iso)

# combinations of countries per species
pairwise <- shared_species |>
  inner_join(shared_species, by = "sciname") |>
  filter(countries_iso.x != countries_iso.y) |>
  count(countries_iso.x, countries_iso.y, name = "shared_species_count")

# 
pairwise_symmetric <- shared_species |>
  inner_join(shared_species, by = "sciname") |>
  filter(countries_iso.x != countries_iso.y) |>
  rowwise() |>
  mutate(
    country1 = min(countries_iso.x, countries_iso.y),
    country2 = max(countries_iso.x, countries_iso.y)
  ) |>
  ungroup() |>
  distinct(sciname, country1, country2) |>
  count(country1, country2, name = "shared_count")


# heatmap
ggplot(pairwise_symmetric, aes(x = country1, y = country2, fill = shared_count)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "inferno", na.value = "white",direction=-1) +
  labs(
    title = "shared threatened and data deficient mammals across countries",
    x = NULL,
    y = NULL,
    fill = "Shared Species"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------------------#
# shared species threathened-----
#----------------------------------------------------------#

# Needs a symmetric matrix


colors <- c(Pakistan = "lightgrey",
            China = "purple", 
            India = "#FF410D",
            Bangladesh = "#6EE2FF", 
            Nepal = "#F7C530",
            Bhutan = "#95CC5E")

chordDiagram(pairwise_symmetric,grid.col = colors,annotationTrack = c("name", "grid"))  

#----------------------------------------------------------#
# network graph-----
#----------------------------------------------------------#

# Create graph
g <- graph_from_data_frame(pairwise_symmetric, directed = FALSE)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = shared_count), alpha = 0.7) +
  geom_node_point(size = 6) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(0.5, 3)) +
  theme_void() +
  labs(title = "Shared Threatened Mammals Between Countries")
