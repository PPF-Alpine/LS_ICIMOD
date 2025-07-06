# Load configuration file
source(here::here("R/00_Config_file.R"))

library(circlize)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggalluvial)


#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/assessment_hkh_mammals_25062025_LS.csv"))

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
    title = "Shared threatened and data deficient mammals across countries",
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

plo1<- chordDiagram(pairwise_symmetric,grid.col = colors,annotationTrack = c("name", "grid"),
                    transparency = 0,direction.type = "diffHeight",)  

# Open SVG device first
svg(filename = paste0(plot_output_path, "shared_threatened_HKH_mammals.svg"),
    width = 8, height = 6)

# Generate chord diagram
chordDiagram(pairwise_symmetric,
             grid.col = colors,
             annotationTrack = c("name", "grid"),
             transparency = 0,
             direction.type = "diffHeight")

# Close device
dev.off()

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

#----------------------------------------------------------#
# ggaluvial :: sankey -----
#----------------------------------------------------------#


# Reorder countries for better visual flow (optional)
pairwise_symmetric <- pairwise_symmetric %>%
  mutate(
    country1 = factor(country1, levels = sort(unique(c(country1, country2)))),
    country2 = factor(country2, levels = sort(unique(c(country1, country2))))
  )


# Create the plot and assign it to a variable
alluvial_plot <- ggplot(pairwise_symmetric,
                        aes(axis1 = country1, axis2 = country2, y = shared_count)) +
  geom_alluvium(aes(fill = country1), width = 1/10, alpha = 0.7) +
  geom_stratum(width = 1/5, fill = "gray95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 4.5, color = "black", fontface = "bold", vjust = 0.5) +
  geom_text(aes(label = shared_count), stat = "alluvium",
            size = 3.5, color = "black", nudge_x = 0.3, check_overlap = TRUE) +
  scale_fill_viridis_d(option = "C") +
  scale_x_discrete(limits = c("Country 1", "Country 2"), expand = c(0.1, 0.05)) +
  theme_void(base_size = 13) +
  theme(legend.position = "none")


plot(alluvial_plot)
plot_output_path <- "C:/Users/lotta/OneDrive - University of Bergen/Desktop/ICIMOD_work/Visualizations/shared_species_alluvial.png"

ggsave(plot_output_path, plot = alluvial_plot, width = 10, height = 12, dpi = 300)


# Save the plot
ggsave(plot_output_path, plot = alluvial_plot, width = 10, height = 12, dpi = 300)

