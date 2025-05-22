# Define the function to get taxonomy and common name
get_gbif_taxonomy <- function(sciname) {
  res <- tryCatch(
    name_backbone(name = sciname),
    error = function(e) return(NULL)
  )
  
  if (!is.null(res) && !is.null(res$scientificName)) {
    tibble(
      sciname = sciname,
      canonical_name = res$canonicalName,
      kingdom = res$kingdom,
      phylum = res$phylum,
      class = res$class,
      order = res$order,
      family = res$family,
      genus = res$genus,
      common_name = res$vernacularName
    )
  } else {
    tibble(
      sciname = sciname,
      canonical_name = NA,
      kingdom = NA,
      phylum = NA,
      class = NA,
      order = NA,
      family = NA,
      genus = NA,
      common_name = NA
    )
  }
}


