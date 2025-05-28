# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(igraph)
library(ggraph)
library(circlize)
library(ggalluvial)


# load RL and HKH list
high_concern_sp <- read.csv(paste0(data_storage_path,"RL_assessments/high_concern_species.csv"))|>
  select(sciname,countries_threatened)

# Split the countries into long format
edges <- high_concern_sp %>%
  separate_rows(countries_threatened, sep = ";\\s*") %>%
  rename(from = sciname, to = countries_threatened)

# Split the countries into long format
edges <- high_concern_sp %>%
  separate_rows(countries_threatened, sep = ";\\s*") %>%
  rename(species = sciname, country = countries_threatened)

# Count shared links
mat <- table(edges$country, edges$species)

# Plot chord diagram
chordDiagram(mat,annotationTrack = c("name", "grid"))


library(circlize)

# Plot the chord diagram with custom settings
chordDiagram(
  mat,
  annotationTrack = c("name", "grid"),
  grid.col = c(  # Assign colors to countries only
    "Nepal" = "tomato",
    "India" = "steelblue",
    "China" = "forestgreen",
    "Bhutan" = "gold",
    "Bangladesh" = "purple",
    "Pakistan" = "darkorange"
  ),
  preAllocateTracks = list(
    track.height = 0.05
  )
)

# Rotate species labels for clarity
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    sector.name = get.cell.meta.data("sector.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    
    circos.text(
      x = mean(xlim),
      y = ylim[1] + 0.2,  # Adjust y offset if needed
      labels = sector.name,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5),
      cex = 0.5             # smaller text
    )
  },
  bg.border = NA
)





# Prepare data for ggalluvial
edges_alluvial <- edges %>%
  count(country, species)

ggplot(edges_alluvial,
       aes(axis1 = species, axis2 = country, y = 1)) +  # one-to-one flows
  geom_alluvium(aes(fill = country), alpha = 0.5) +  # slightly thinner flows
  geom_stratum(width = 0.2, fill = "grey90", color = "grey40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_x_discrete(limits = c("Species", "Country"), expand = c(.05, .05)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank()
  )



library(tidyverse)

# Create presence/absence matrix
heat_df <- edges %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = country, values_from = value, values_fill = 0)

# Convert to long format for ggplot
heat_long <- heat_df %>%
  pivot_longer(-species, names_to = "country", values_to = "present")

ggplot(heat_long, aes(x = country, y = species, fill = present)) +
  geom_tile(color = "white", width = 1) +  # adjust width here (default is 1)
  scale_fill_gradient(low = "white", high = "darkgrey") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10)
  )

ggplot(heat_long, aes(x = country, y = species, fill = present)) +
  geom_tile(color = "white",width=0.8) +
  scale_fill_gradient(low = "white", high = "darkgrey") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),  # smaller text
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )
