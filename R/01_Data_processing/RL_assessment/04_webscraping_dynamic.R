#################
#----------------------------------------------------------#

# Load configuration file
source(here::here("R/00_Config_file.R"))

# install.packages("rvest")

library(rvest)
library(httr2)
library(tidyverse)
library(glue)
#----------------------------------------------------------#
# DONT TOUCH THIS WORKS ---
#----------------------------------------------------------#

# Base URL 
base_url <- "https://www.nationalredlist.org/assessments?search_api_fulltext=&field_year_assessed=&field_sco=&field_phylum=&field_class=&field_order=&field_family=&field_genus=&page="

# How many pages to scrape (there is >900 pages in total without any filters..)
limit <- 931 

# Store tables
all_tables <- list()

for (i in 0:(limit - 1)) {
  url <- glue("{base_url}{i}")
  cat("Scraping page:", i + 1, "\n")
  
  page <- read_html(url)
  table <- page |>
    html_element("table") |>
    html_table(fill = TRUE)
  
  if (!is.null(table)) {
    # Clean column names
    names(table) <- trimws(gsub("\n.*", "", names(table)))
    all_tables[[i + 1]] <- table
  } else {
    message("No table found on page ", i)
  }
  
  Sys.sleep(1)
}

# Combine everything
final_df <- bind_rows(all_tables)

write.csv(final_df,paste0(data_storage_path,"RL_assessments/national_assessment_webscraping_output.csv"))


# --> IMPROVE: find a way to filter


