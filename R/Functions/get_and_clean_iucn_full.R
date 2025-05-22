flatten_assessment_full <- function(assessment, genus, species) {
  if (is.null(assessment) || length(assessment) == 0) {
    return(tibble(
      sciname = paste(genus, species),
      year_published = NA,
      criteria = NA,
      citation = NA,
      red_list_version = NA,
      red_list_code = NA,
      threats_code = NA,
      threats_description = NA,
      stresses_code = NA,
      stresses_description = NA,
      common_name = NA,
      order_name = NA,
      family_name = NA,
      pop_trend_global = NA,
      range = NA,
      habitats_iucn = NA,
      threats_summary = NA,
      country_code_global = NA
    ))
  }
  
  get_or_na <- function(x) {
    if (is.null(x)) return(NA)
    if (length(x) == 0) return(NA)
    return(x)
  }
  
  safe_extract <- function(x, ...) {
    tryCatch(purrr::pluck(x, ...), error = function(e) NA)
  }
  
  tibble(
    sciname = paste(genus, species),
    year_published = get_or_na(assessment$year_published),
    criteria = get_or_na(assessment$criteria),
    citation = get_or_na(assessment$citation),
    red_list_version = safe_extract(assessment, "red_list_category", "version"),
    red_list_code = safe_extract(assessment, "red_list_category", "code"),
    threats_code = paste(get_or_na(safe_extract(assessment, "threats", "code")), collapse = "; "),
    threats_description = paste(get_or_na(safe_extract(assessment, "threats", "description")), collapse = "; "),
    stresses_code = paste(get_or_na(safe_extract(assessment, "stresses", "code")), collapse = "; "),
    stresses_description = paste(get_or_na(safe_extract(assessment, "stresses", "description")), collapse = "; "),
    common_name = {
      cn <- safe_extract(assessment, "taxon", "common_names")
      if (is.data.frame(cn) && "name" %in% names(cn)) {
        paste(unique(na.omit(cn$name)), collapse = "; ")
      } else {
        NA
      }
    },
    
    order_name = safe_extract(assessment, "taxon", "order_name"),
    family_name = safe_extract(assessment, "taxon", "family_name"),
    pop_trend_global = safe_extract(assessment, "population_trend", "description", "en"),
    range = safe_extract(assessment, "documentation", "range"),
    habitats_iucn = paste(get_or_na(safe_extract(assessment, "documentation", "habitats")), collapse = "; "),
    threats_summary = paste(get_or_na(safe_extract(assessment, "documentation", "threats")), collapse = "; "),
    country_code_global = paste(get_or_na(safe_extract(assessment, "locations", "code")), collapse = "; ")
  )
}


get_and_clean_iucn_full <- function(genus, species) {
  Sys.sleep(1.5)  # Avoid rate limiting
  tryCatch({
    res <- rl_species_latest(genus = genus, species = species, key = iucn_key)
    flatten_assessment_full(res, genus, species)
  }, error = function(e) {
    message(paste("No IUCN assessment available for", genus, species, ":", e$message))
    flatten_assessment_full(NULL, genus, species)
  })
}
