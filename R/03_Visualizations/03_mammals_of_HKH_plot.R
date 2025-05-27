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

length(unique(full_assessment_hkh_mammals$sciname))

# Clean and reshape data
df_long <- full_assessment_hkh_mammals %>%
  select(sciname, min = average_min_elevation, max = average_max_elevation) %>%
  drop_na(min, max) %>%
  mutate(mid_elevation = (min + max) / 2,
         range = "HKH")|>
  distinct()

df_long_jittered <- df_long %>%
  mutate(jittered_mid = mid_elevation + runif(n(), min = -100, max = 100))

#----------------------------------------------------------#
# elevational range plot -----
#----------------------------------------------------------#

# Plot 1: Elevation ranges with midpoints
# Define the output directory for plots
plot_output_path <- paste0(data_storage_path, "Visualizations/")

# Plot 1: Elevational ranges
plot1 <- ggplot(df_long_jittered) +
  geom_linerange(aes(x = jittered_mid, ymin = min, ymax = max),
                 alpha = 0.8, linewidth = 0.3, color = "darkgrey") +
  geom_point(aes(x = jittered_mid, y = jittered_mid),
             size = 0.7, alpha = 0.9, color = "darkgrey") +
  scale_y_continuous(name = "Elevation range (m)") +
  scale_x_continuous(name = "Midpoint elevation (m)") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12)) 


# Save Plot 1
ggsave(filename = paste0(plot_output_path, "elevational_ranges_HKH_mammals.jpg"),
       plot = plot1, width = 8, height = 6, dpi = 300)

# Plot 2: Midpoint elevations
plot2 <- ggplot(df_long, aes(x = range, y = mid_elevation)) +
  geom_violin(fill = "darkgrey", alpha = 0.6, color = NA, scale = "width") +
  geom_jitter(width = 0.2, size = 0.7, alpha = 0.8, color = "black") +
  scale_y_continuous(name = "Midpoint elevation (m)") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank()) 

#----------------------------------------------------------#
# save plots -----
#----------------------------------------------------------#
# Save Plot 2
ggsave(filename = paste0(plot_output_path, "midpoint_elevations_HKH_mammals.jpg"),
       plot = plot2, width = 8, height = 6, dpi = 300)


# Combine plots with a shared legend and title alignment
combined_plot <- plot1 + plot2 +
  plot_layout(ncol = 2) 

# Show the plot
print(combined_plot)

# Save the combined plot
ggsave(filename = paste0(plot_output_path, "combined_elevations.jpg"),
       plot = combined_plot, width = 15, height = 6, dpi = 300)


#----------------------------------------------------------#
# threathened at which elevations-----
#----------------------------------------------------------#
df_long <- full_assessment_hkh_mammals %>%
  select(sciname, min = average_min_elevation, max = average_max_elevation,status_summary_national) %>%
  drop_na(min, max) %>%
  mutate(mid_elevation = (min + max) / 2,
         range = "HKH")|>
  distinct()


df_long_jittered <- df_long %>%
  mutate(jittered_mid = mid_elevation + runif(n(), min = -100, max = 100))

plot1 <- ggplot(df_long_jittered) +
  geom_linerange(
    aes(
      x = jittered_mid,
      ymin = min,
      ymax = max,
      color = case_when(
        status_summary_national == "threatened" ~ "threatened",
        status_summary_national %in% c("data deficient", "not evaluated") ~ "dd_ne",
        TRUE ~ "not threathened"
      )
    ),
    alpha = 0.8,
    linewidth = 0.3
  ) +
  
  scale_color_manual(
    name = "Status",
    values = c(
      "threatened" = "red",
      "dd_ne" = "black",
      "not threathened" = "darkgrey"
    ),
    labels = c(
      "threatened" = "Threatened",
      "dd_ne" = "Data Deficient / Not Evaluated",
      "not threathened" = "not threathened"
    )
  ) +
  
  scale_y_continuous(name = "Elevation range (m)") +
  scale_x_continuous(name = "Midpoint elevation (m)") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12))


# violin plot 
plot2 <- ggplot(df_long, aes(x = range, y = mid_elevation)) +
  # Violin background
  geom_violin(fill = "darkgrey", alpha = 0.6, color = NA, scale = "width") +
  
  # Jittered points colored by threat status
  geom_jitter(
    aes(color = case_when(
      status_summary_national == "threatened" ~ "threatened",
      status_summary_national %in% c("data deficient", "not evaluated") ~ "dd_ne",
      TRUE ~ "not threathened"
    )),
    width = 0.2,
    size = 1.1,
    alpha = 0.8
  ) +
  
  # Custom color scale
  scale_color_manual(
    name = "Status",
    values = c(
      "threatened" = "red",
      "dd_ne" = "black",
      "not threathened" = "darkgrey"
    ),
    labels = c(
      "threatened" = "Threatened",
      "dd_ne" = "Data Deficient / Not Evaluated",
      "not threathened" = "not threathened"
    )
  ) +
  
  # Axis formatting
  scale_y_continuous(name = "Midpoint elevation (m)") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank()
  )


plot(plot2)

combined_plot <- plot1 + plot2 +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

# Show the plot
print(combined_plot)

# Save the combined plot
ggsave(
  filename = paste0(plot_output_path, "combined_elevations_threatened.jpg"),
  plot = combined_plot,
  width = 15,
  height = 6,
  dpi = 300
)
