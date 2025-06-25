library(dplyr)
library(ggplot2)
library(FSA)
library(MASS)


# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#
# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/assessment_hkh_mammals_10062025_LS.csv"))

length(unique(full_assessment_hkh_mammals$sciname))

df_long <- full_assessment_hkh_mammals %>%
  select(sciname, min = average_min_elevation, max = average_max_elevation,status_summary_national) %>%
  drop_na(min, max) %>%
  mutate(mid_elevation = (min + max) / 2,
         range = "HKH")|>
  distinct()

# Prepare data
df_clean <- df_long %>%
  mutate(
    status_summary_national = fct_explicit_na(as.factor(status_summary_national), na_level = "not evaluated"),
    mid_elevation = (min + max) / 2,
    elev_range = max-min
  )

#----------------------------------------------------------#
# relationship status threathened and midpoint elev --
#----------------------------------------------------------#
df_clean %>%
  group_by(status_summary_national) %>%
  summarise(
    median_range = median(mid_elevation, na.rm = TRUE),
    mean_range = mean(mid_elevation, na.rm = TRUE),
    sd_range = sd(mid_elevation, na.rm = TRUE),
    n = n()
  )

plot1 <- ggplot(df_clean, aes(x = status_summary_national, y = mid_elevation, fill = status_summary_national)) +
  geom_violin(color = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.2, color = "black") +
  scale_fill_manual(
    values = c(
      "data deficient" = "grey80",
      "not evaluated" = "grey40",
      "not threatened" = "gold",
      "threatened" = "darkred"
    )
  ) +
  labs(
    x = "Conservation status (national)", 
    y = "Midpoint elevation (m)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.line = element_line(color = "black"),
    panel.border = element_blank(),
    legend.position = "none"  # Remove if you don't need the legend
  )

plot(plot1)

# Kruskal-Wallis test (includes NA as a group)
kruskal_test <- kruskal.test(mid_elevation ~ status_summary_national, data = df_clean)
print(kruskal_test)

# Dunn's post-hoc test (includes NA group)
dunn_test <- dunnTest(mid_elevation ~ status_summary_national, data = df_clean, method = "bh")
print(dunn_test)

plot_output_path <- paste0(data_storage_path, "Visualizations/")
# Save the combined plot
ggsave(
  filename = paste0(plot_output_path, "mid_elev_threathened.jpg"),
  plot = plot1,
  width = 15,
  height = 6,
  dpi = 300
)

#----------------------------------------------------------#
# relationship status threathened and elev range--
#----------------------------------------------------------#
df_clean %>%
  group_by(status_summary_national) %>%
  summarise(
    median_range = median(elev_range, na.rm = TRUE),
    mean_range = mean(elev_range, na.rm = TRUE),
    sd_range = sd(elev_range, na.rm = TRUE),
    n = n()
  )

library(ggplot2)

library(ggplot2)

# Customized violin plot with status-based fill colors
plot2 <- ggplot(df_clean, aes(x = status_summary_national, y = elev_range, fill = status_summary_national)) +
  geom_violin(color = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.2, color = "black") +
  scale_fill_manual(
    values = c(
      "data deficient" = "grey80",
      "not evaluated" = "grey40",
      "not threatened" = "gold",
      "threatened" = "darkred"
    )
  ) +
  labs(
    x = "Conservation status (national)", 
    y = "Elevational range (m)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.line = element_line(color = "black"),
    panel.border = element_blank(),
    legend.position = "none"  # Remove if you don't need the legend
  )

plot(plot2)


# Save the combined plot
ggsave(
  filename = paste0(plot_output_path, "elev_range_threathened.jpg"),
  plot = plot2,
  width = 15,
  height = 6,
  dpi = 300
)

# Kruskal-Wallis test (includes NA as a group)
kruskal_test <- kruskal.test(elev_range ~ status_summary_national, data = df_clean)
print(kruskal_test)

# Dunn's post-hoc test (includes NA group)
dunn_test <- dunnTest(elev_range ~ status_summary_national, data = df_clean, method = "bh")
print(dunn_test)

summary(lm(elev_range ~ status_summary_national, data = df_clean))


library(patchwork)

# Remove x-axis labels and text from the top plot (plot1)
plot1_clean <- plot1 + theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

# Combine vertically
combined_plot <- plot1_clean / plot2  # plot2 retains full x-axis

# Display
combined_plot

# Save
ggsave(
  filename = paste0(plot_output_path, "elev_range_midpoint_combined.jpg"),
  plot = combined_plot,
  width = 12,
  height = 9,
  dpi = 300
)
