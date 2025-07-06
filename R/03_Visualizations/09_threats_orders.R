# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
unique(full_assessment_hkh_mammals$countries_iso)

# load RL and HKH list
full_assessment_hkh_mammals <- read.csv(paste0(data_storage_path,"RL_assessments/assessment_hkh_mammals_25062025_LS.csv"))|>
  filter(
    str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore"
  )

#----------------------------------------------------------#
# overview threathened species -----
#----------------------------------------------------------#

# threat by order 
order_threat<-full_assessment_hkh_mammals|>
  group_by(order,status_code_national)|>
  count()

# threat by order 
order_threat<-full_assessment_hkh_mammals |>
  filter(
    status_summary_national == "threatened"|status_summary_national =="data deficient"
  ) |>
  group_by(order)|>
  count()


# Set factor order for status codes
status_levels <- c("DD", "NE", "NA", "RE", "CR", "EN", "VU", "NT", "LC")

order_threats_plot <- order_threat |>
  mutate(
    status_code_national = factor(status_code_national, levels = status_levels),
    order = fct_reorder(order, n, .fun = sum, .desc = TRUE)
  )|>
  filter(order!="NA")


# Plot
ggplot(order_threats_plot, aes(x = n, y = order, fill = status_code_national)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(
    option = "inferno",
    na.value = "lightgrey",
    direction = 1
  ) +
  labs(
    x = "Number of listings in national assessments",
    y = NULL,
    fill = "Status",
  ) +
  theme_minimal()


#----------------------------------------------------------#
# othreathened by order -----
#----------------------------------------------------------#

order_threat<-full_assessment_hkh_mammals|>
  distinct(sciname,order,status_code_national)|>
  group_by(order,status_code_national)|>
  count()

status_levels <- c("DD", "NE", "NA", "RE", "CR", "EN", "VU", "NT", "LC")

order_threat <- order_threat |>
  mutate(status_code_national = factor(status_code_national, levels = status_levels))


ggplot(order_threat, aes(x = status_code_national, y = n, fill = status_code_national)) +
  geom_col() +
  scale_fill_viridis_d(option = "inferno", direction = 1, na.value = "lightgrey") +
  facet_wrap(~order, scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    x = NULL,
    y = "Number of listings across HKH countries",
    fill = "Status",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  )

order_threat_prop <- full_assessment_hkh_mammals |>
  group_by(order, status_code_national) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(order) |>
  mutate(
    total = sum(n),
    proportion = n / total
  )

ggplot(order_threat_prop, aes(x = reorder(order, -total), y = proportion, fill = status_code_national)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "inferno", direction = 1, na.value = "lightgrey") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Taxonomic Order",
    y = "Proportion of Records",
    fill = "Status",
    title = "Threat Status Composition by Order (Row-Based Proportions)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#----------------------------------------------------------#
# shared orders threathened -----
#----------------------------------------------------------#
full_ass_work_data <- full_assessment_hkh_mammals|>
  filter(str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore")|>
  select(sciname,order,family,status_code_national,status_summary_national,status_code_global,countries_iso)


n_total_species <- full_ass_work_data |>
  distinct(sciname) |>
  nrow()

# Unique species per order
species_per_order <- full_ass_work_data |>
  distinct(sciname, order) |>
  count(order, name = "n_species")

# Unique species per family
species_per_family <- full_ass_work_data |>
  distinct(sciname, family) |>
  count(family, name = "n_species")







### 

shared_orders <- full_ass_work_data |>
  filter(status_summary_national %in% c("threatened", "data deficient")) |>
  separate_rows(countries_iso, sep = ",\\s*") |>
  distinct(sciname, order, countries_iso)

# Step 2: Join with itself to get country pairs for each species and order
shared_order_pairs <- shared_orders |>
  inner_join(shared_orders, by = c("sciname", "order")) |>
  filter(countries_iso.x != countries_iso.y) |>
  rowwise() |>
  mutate(
    country1 = min(countries_iso.x, countries_iso.y),
    country2 = max(countries_iso.x, countries_iso.y)
  ) |>
  ungroup()

# Step 3: Count unique shared species in each order per country pair
order_country_pair_counts <- shared_order_pairs |>
  distinct(sciname, order, country1, country2) |>
  count(country1, country2, order, name = "shared_species_count") |>
  arrange(desc(shared_species_count))

order_country_pair_counts <- order_country_pair_counts |>
  mutate(country_pair = paste(country1, country2, sep = " – "))

# Plot heatmap
ggplot(order_country_pair_counts, aes(x = country_pair, y = order, fill = shared_species_count)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno",direction=-1) +
  labs(
    x = NULL,
    y = NULL,
    fill = "shared species",
    title = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(order_country_pair_counts, aes(x = reorder(paste(country1, country2, sep = " – "), shared_species_count), y = shared_species_count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~order, scales = "free_x") +
  labs(
    x = "Country Pair",
    y = "Shared Species",
    title = "Shared Species per Order by Country Pair"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

