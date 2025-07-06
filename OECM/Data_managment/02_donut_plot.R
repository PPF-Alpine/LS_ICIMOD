# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(webr)


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
  )



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
  arrange(theme, desc(frequency))


#----------------------------------------------------------#
# biodiversity and cultural value
#----------------------------------------------------------#

biod_cult_val <- quotation_summary |>
  filter(theme %in% c("Biodiversity value and its conservation"))|>
  left_join(oecm_codes|>
              select(codegroup_1,codes))|>
  filter(!is.na(codegroup_1))



PieDonut(
  data = biod_cult_val,
  mapping = aes(x = codegroup_1, y = codes, count = frequency),
  ratioByGroup = FALSE,
  explodePie = TRUE,
  showPieName = FALSE,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  addPieLabel =FALSE,
  maxx=2,
  donutLabelSize = 4,
  pieLabelSize = 0,
  pieAlpha = 0.6,
  labelposition = 0.1
)


#----------------------------------------------------------#
# oecm perception
#----------------------------------------------------------#

oecm <- quotation_summary |>
  filter(theme %in% c("Benefits regarding OECM","Challenges and concerns regarding OECM"))


# Use PieDonut on your data
oecm$theme <- factor(oecm$theme)

library(ggplot2)

# Save PieDonut as object
PieDonut(
  data = oecm,
  mapping = aes(x = theme, y = codes, count = frequency, fill = theme),
  explodePie = TRUE,
  explode = c(1, 1),
  ratioByGroup = FALSE,
  #maxx=2,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  addPieLabel = FALSE,
  donutLabelSize = 4,
  pieLabelSize = 0,
  pieAlpha = 0.6,
  labelposition = 0.1
)

plot_output_path <- paste0(data_storage_path, "Visualizations/")
ggsave(
  filename = paste0(plot_output_path, "oecm_donut.png"),
  plot = p,
  width = 25,
  height = 25,
  dpi = 300
)

ggsave(
  filename = paste0(plot_output_path, "oecm_donut.pdf"),
  plot = p,
  width = 10,
  height = 10,
  bg = "white"
)


unique(oecm$theme)
table(oecm$theme)
# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(webr)


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
  )



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
  arrange(theme, desc(frequency))


#----------------------------------------------------------#
# biodiversity and cultural value
#----------------------------------------------------------#

biod_cult_val <- quotation_summary |>
  filter(theme %in% c("Biodiversity value and its conservation"))|>
  left_join(oecm_codes|>
              select(codegroup_1,codes))|>
  filter(!is.na(codegroup_1))




a<-PieDonut(
  data = biod_cult_val,
  mapping = aes(x = codegroup_1, y = codes, count = frequency),
  explode = 2,
  showPieName = FALSE,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  addPieLabel =FALSE,
  maxx=2,
  donutLabelSize = 10,
  pieLabelSize = 0,
  pieAlpha = 0.6,
  labelposition = 0.1)

plot_output_path <- paste0(data_storage_path, "Visualizations/")
ggsave(
  filename = paste0(plot_output_path, "biodiv_donut.pdf"),
  plot = a,
  width = 25,
  height = 25,
  dpi = 300
)
#----------------------------------------------------------#
# oecm perception
#----------------------------------------------------------#

oecm <- quotation_summary |>
  filter(theme %in% c("Benefits regarding OECM","Challenges and concerns regarding OECM"))


# Use PieDonut on your data
oecm$theme <- factor(oecm$theme)

library(ggplot2)

# Save PieDonut as object
PieDonut(
  data = oecm,
  mapping = aes(x = theme, y = codes, count = frequency, fill = theme),
  explodePie = TRUE,
  explode = c(1, 1),
  ratioByGroup = FALSE,
  #maxx=2,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  addPieLabel = FALSE,
  donutLabelSize = 4,
  pieLabelSize = 0,
  pieAlpha = 0.6,
  labelposition = 0.1
)

plot_output_path <- paste0(data_storage_path, "Visualizations/")
ggsave(
  filename = paste0(plot_output_path, "oecm_donut.png"),
  plot = p,
  width = 25,
  height = 25,
  dpi = 300
)

ggsave(
  filename = paste0(plot_output_path, "oecm_donut.pdf"),
  plot = p,
  width = 10,
  height = 10,
  bg = "white"
)


unique(oecm$theme)
table(oecm$theme)
