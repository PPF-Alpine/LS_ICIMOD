flatten_assessment_elev <- function(test, genus, species) {
  if (is.null(test) || length(test) == 0) {
    return(tibble(
      sciname = paste(genus, species),
      order = NA,
      family = NA,
      upper_limit = NA_real_,
      lower_limit = NA_real_
    ))
  }
  
  get_or_na <- function(x) if (is.null(x)) NA else x
  
  sciname <- get_or_na(test$taxon$scientific_name)
  order <- get_or_na(test$taxon$order_name)
  family <- get_or_na(test$taxon$family_name)
  upper_limit <- as.numeric(get_or_na(test$supplementary_info$upper_elevation_limit))
  lower_limit <- as.numeric(get_or_na(test$supplementary_info$lower_elevation_limit))
  
  tibble(
    sciname = sciname,
    order = order,
    family = family,
    upper_limit = upper_limit,
    lower_limit = lower_limit
  )
}




get_and_clean_iucn_elevations <- function(genus, species) {
  Sys.sleep(1.5)
  tryCatch({
    res <- rl_species_latest(genus = genus, species = species, key = iucn_key)
    flatten_assessment_elev(res, genus, species)
  }, error = function(e) {
    message(paste("No IUCN assessment available for", genus, species, ":", e$message))
    flatten_assessment_elev(NULL, genus, species)
  })
}
