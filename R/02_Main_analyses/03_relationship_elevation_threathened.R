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
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_02062025.csv"))

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

# Boxplot of mid elevation by status (including NA)
plot1 <- ggplot(df_clean, aes(x = status_summary_national, y = mid_elevation)) +
  geom_violin(trim = FALSE, fill = "lightgray") +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
  labs(x = "Conservation Status (National)", 
       y = "Mid Elevation (m)",
       title = "Mid elevation vs. national conservation status") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

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
# relationship status threathened and midpoint elev --
#----------------------------------------------------------#
df_clean %>%
  group_by(status_summary_national) %>%
  summarise(
    median_range = median(elev_range, na.rm = TRUE),
    mean_range = mean(elev_range, na.rm = TRUE),
    sd_range = sd(elev_range, na.rm = TRUE),
    n = n()
  )

# Boxplot of mid elevation by status (including NA)
plot2<-ggplot(df_clean, aes(x = status_summary_national, y = elev_range)) +
  geom_violin(fill = "lightgray") +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
  labs(x = "Conservation Status (National)", 
       y = "elevarional range (m)",
       title = "Elevational range vs. national conservation status") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

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
