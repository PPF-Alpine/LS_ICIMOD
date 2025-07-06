# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(webr)
library(stringr)
#install.packages("Cairo")  # if not yet installed
library(Cairo)

# Save plot
plot_output_path <- paste0(data_storage_path, "Visualizations/")

oecm_quotations <- readxl::read_xlsx(paste0(data_storage_path,"OECM/Interview_Codes/ICIMOD OECM-quotations.xlsx"))|>
  filter(!is.na(codes))

oecm_codes <- readxl::read_xlsx(paste0(data_storage_path,"OECM/Interview_Codes/ICIMOD OECM-codes.xlsx"))|>
  janitor::clean_names()|>
  mutate(
    codes = if_else(!str_starts(name, "Theme:"), paste0("Theme:", name), name),  # reattach "Theme:" if dropped
    theme = str_extract(name, "Theme: ([^:]+)") |> 
      str_remove("Theme: ") |> 
      str_trim(),
    codes = str_remove(name, "Theme: [^:]+: ") |> 
      str_trim()
  ) |>
  filter(codegroup_1 == "add_to_figure" | codegroup_2 == "add_to_figure")## filter for relevant codes 


oecm_quotations_cleaned <- oecm_quotations |>
  filter(!is.na(codes)) |>
  separate_rows(codes, sep = ", Theme:") |>  # split only at ", Theme:"
  mutate(
    codes = if_else(!str_starts(codes, "Theme:"), paste0("Theme:", codes), codes),  # reattach "Theme:" if dropped
    theme = str_extract(codes, "Theme: ([^:]+)") |> 
      str_remove("Theme: ") |> 
      str_trim(),
    codes = str_remove(codes, "Theme: [^:]+: ") |> 
      str_trim()
  ) |>
  arrange(theme, codes) |>
  mutate(row_number = row_number())

n_interviews <- oecm_quotations_cleaned %>%
  distinct(document) %>%
  nrow()

quotation_summary <- oecm_quotations_cleaned |>
  count(theme, codes, name = "frequency") |>
  arrange(theme, desc(frequency)) |>
  filter(codes %in% oecm_codes$codes) |>
  mutate(proportion = frequency / n_interviews)

writexl::write_xlsx(quotation_summary, path = paste0(plot_output_path, "quotation_summary.xlsx"))
#----------------------------------------------------------#
# biodiversity and cultural value circular barplot
#----------------------------------------------------------#

biod_cult_val <- quotation_summary |>
  filter(theme %in% c("Biodiversity value and its conservation","Cultural values of the area"))|>
  left_join(oecm_codes|>
              select(codegroup_1,codegroup_2,codes),by="codes")|>
  mutate(theme_fine = case_when(
    codegroup_1 == "positive" ~ "Nature value",
    codegroup_2 == "positive" ~ "Nature value",
    codegroup_1 == "negative" ~ "Conservation challenges",
    theme == "Cultural values of the area" ~ "Cultural value",  # this won't be reached
    TRUE ~ NA_character_
  ))

# Wrap code labels for readability
biod_cult_val$codes_wrapped <- str_wrap(biod_cult_val$codes, width = 19)

# Add an index for positioning
biod_cult_val <- biod_cult_val %>%
  arrange(theme_fine, desc(frequency)) %>%
  mutate(
    codes_wrapped = factor(codes_wrapped, levels = codes_wrapped),
    id = as.numeric(codes_wrapped)
  )


plt <- ggplot(biod_cult_val, aes(
  x = codes_wrapped,
  y = frequency,
  fill = theme_fine
)) +
  # Radial guide lines from center to bar
  geom_segment(aes(x = codes_wrapped, xend = codes_wrapped, y = 0, yend = max(frequency) + 1),
               color = "grey90", size = 0.3) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.9) +
  coord_polar(theta = "x") +
  scale_y_continuous(breaks = seq(0, max(biod_cult_val$frequency), by = 5)) +
  scale_fill_brewer(palette = "BuGn") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),       # remove y axis text
    axis.ticks.y = element_blank(),      # remove y axis ticks
    axis.line.y = element_blank(),       # remove y axis line
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    panel.grid = element_blank(),
    legend.position = "bottom",          # move legend below plot
    legend.direction = "horizontal",     # horizontal legend layout
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) 

plot(plt)
# Save the plot as PNG
ggsave(
  filename = paste0(plot_output_path, "biodiv_cultural_value_plot.png"),
  plot = plt,
  width = 15, height = 13, units = "in", dpi = 300
)
#----------------------------------------------------------#
# biodiversity and cultural value circular barplot
#----------------------------------------------------------#
# Filter for OECM-related themes
managment <- quotation_summary %>%
  filter(theme %in% c("Community engagment for OECM","Managment and governance of OECM"))

# Wrap code labels
managment$codes_wrapped <- str_wrap(managment$codes, width = 20)

# Order bars by theme then frequency
managment <- managment %>%
  arrange(theme, desc(frequency)) %>%
  mutate(
    codes_wrapped = factor(codes_wrapped, levels = codes_wrapped),
    id = as.numeric(codes_wrapped)  # for segment drawing
  )

# Determine max frequency for guide lines
max_freq <- max(managment$frequency)

# Create the circular barplot
plt <- ggplot(managment, aes(
  x = codes_wrapped,
  y = frequency,
  fill = theme
)) +
  # Radial guide lines from center to bar
  geom_segment(aes(x = codes_wrapped, xend = codes_wrapped, y = 0, yend = max(frequency) + 1),
               color = "grey90", size = 0.3) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.9) +
  coord_polar(theta = "x") +
  scale_y_continuous(breaks = seq(0, max(managment$frequency), by = 5)) +
  scale_fill_brewer(palette = "Purples") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),       # remove y axis text
    axis.ticks.y = element_blank(),      # remove y axis ticks
    axis.line.y = element_blank(),       # remove y axis line
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    panel.grid = element_blank(),
    legend.position = "bottom",          # move legend below plot
    legend.direction = "horizontal",     # horizontal legend layout
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) 

plot(plt)
# Save the plot as PNG
ggsave(
  filename = paste0(plot_output_path, "managment_plot.png"),
  plot = plt,
  width = 15, height = 13, units = "in", dpi = 300
)

#----------------------------------------------------------#
# biodiversity and cultural value circular barplot
#----------------------------------------------------------#
# Filter for OECM-related themes
oecm <- quotation_summary %>%
  filter(theme %in% c("Benefits regarding OECM", "Challenges and concerns regarding OECM"))

# Wrap code labels
oecm$codes_wrapped <- str_wrap(oecm$codes, width = 18)

# Order bars by theme then frequency
oecm <- oecm %>%
  arrange(theme, desc(frequency)) %>%
  mutate(
    codes_wrapped = factor(codes_wrapped, levels = codes_wrapped),
    id = as.numeric(codes_wrapped)  # for segment drawing
  )

# Determine max frequency for guide lines
max_freq <- max(managment$frequency)

# Create the circular barplot
plt <- ggplot(oecm, aes(
  x = codes_wrapped,
  y = frequency,
  fill = theme
)) +
  # Radial guide lines from center to bar
  geom_segment(aes(x = codes_wrapped, xend = codes_wrapped, y = 0, yend = max(frequency) + 1),
               color = "grey90", size = 0.3) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.9) +
  coord_polar(theta = "x") +
  scale_y_continuous(breaks = seq(0, max(biod_cult_val$frequency), by = 5)) +
  scale_fill_brewer(palette = "Oranges") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),       # remove y axis text
    axis.ticks.y = element_blank(),      # remove y axis ticks
    axis.line.y = element_blank(),       # remove y axis line
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    panel.grid = element_blank(),
    legend.position = "bottom",          # move legend below plot
    legend.direction = "horizontal",     # horizontal legend layout
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) 

plot(plt)
# Save the plot as PNG
ggsave(
  filename = paste0(plot_output_path, "oecmbenefits_plot.png"),
  plot = plt,
  width = 15, height = 13, units = "in", dpi = 300
)




