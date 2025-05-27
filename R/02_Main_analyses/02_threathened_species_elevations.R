# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
unique(full_assessment_hkh_mammals$countries_iso)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/full_assessment_hkh_mammals_27052025.csv"))|>
  filter(
    str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore"
  )


