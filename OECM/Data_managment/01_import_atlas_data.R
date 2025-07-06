# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(webr)
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


quotation_summary <- oecm_quotations_cleaned |>
  count(theme, codes, name = "frequency") |>
  arrange(theme, desc(frequency))|>
  filter(codes%in%oecm_codes$codes)


#----------------------------------------------------------#
# biodiversity and cultural value
#----------------------------------------------------------#

biod_cult_val <- quotation_summary |>
  filter(theme %in% c("Biodiversity value and its conservation","Cultural values of the area"))|>
  left_join(oecm_codes|>
              select(codegroup_1,codegroup_2,codes),by="codes")|>
  mutate(theme_fine = case_when(
    codegroup_1 == "positive" ~ "biodiversity value positive",
    codegroup_1 == "negative" ~ "biodiversity value negative",
    theme == "Cultural values of the area" ~ "cultural value",  # this won't be reached
    TRUE ~ NA_character_
  ))



PieDonut(
  data = biod_cult_val,
  mapping = aes(x = theme_fine, y = codes, count = frequency),
  ratioByGroup = FALSE,
  explodePie = TRUE,
  showPieName = FALSE,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  addPieLabel =FALSE,
  maxx=2.6,
  donutLabelSize = 1,
  pieLabelSize = 0,
  pieAlpha = 0.6,
  labelposition = 0.05
)+
  theme(
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),  # Adjust margins around the chart
    plot.background = element_rect(fill = "white"),  # Set background to white
    panel.background = element_rect(fill = "white")  # Set panel background to white
  )
recorded_plot <- recordPlot()
# Save using Cairo SVG device
CairoSVG(
  filename = file.path(plot_output_path, "biodiv_donut.svg"),
  width = 10, height = 10
)
replayPlot(recorded_plot)
dev.off()
#----------------------------------------------------------#
# oecm perception
#----------------------------------------------------------#

oecm <- quotation_summary |>
  filter(theme %in% c("Benefits regarding OECM","Challenges and concerns regarding OECM"))



# Save PieDonut as object
PieDonut(
  data = oecm,
  mapping = aes(x = theme, y = codes, count = frequency),
  ratioByGroup = FALSE,
  explodePie = TRUE,
  showPieName = FALSE,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  addPieLabel =FALSE,
  maxx=2.6,
  donutLabelSize = 2,
  pieLabelSize = 0,
  pieAlpha = 0.6,
  labelposition = 0.05
)



recorded_plot <- recordPlot()
# Save using Cairo SVG device
CairoSVG(
  filename = file.path(plot_output_path, "oecm_donut_2.svg"),
  width = 10, height = 10
)
replayPlot(recorded_plot)
dev.off()

recorded_plot <- recordPlot()
# Save using Cairo SVG device
CairoSVG(
  filename = file.path(plot_output_path, "oecm_donut_2.svg"),
  width = 10, height = 10
)
replayPlot(recorded_plot)
dev.off()


#----------------------------------------------------------#
# oecm managment 
#----------------------------------------------------------#

oecm_managment <- quotation_summary |>
  filter(theme %in% c("Community engagment for OECM","Managment and governance of OECM"))



# Save PieDonut as object
PieDonut(
  data = oecm_managment,
  mapping = aes(x = theme, y = codes, count = frequency, fill = theme),
  explodePie = TRUE,
  explode = 1,
  ratioByGroup = FALSE,
  maxx=3,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  addPieLabel = FALSE,
  donutLabelSize = 4,
  pieLabelSize = 0,
  labelpositionThreshold = 0,
  pieAlpha = 0.6,
  labelposition = 0.1
)

#----------------------------------------------------------#
# Managment and community needs
#----------------------------------------------------------#

oecm <- quotation_summary |>
  filter(theme %in% c("Benefits regarding OECM","Challenges and concerns regarding OECM"))

