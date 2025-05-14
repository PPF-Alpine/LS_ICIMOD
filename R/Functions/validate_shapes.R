# validate shapes individually ✅
validate_shapes_individually <- function(species_shapes) {
  species_shapes <- sf::st_as_sf(species_shapes)
  message("Checking for invalid shapes...")
  
  for (i in 1:nrow(species_shapes)) {
    if (!sf::st_is_valid(species_shapes[i, ])) {
      message(paste("Invalid shape found at row", i, ". Making invalid species valid."))
      species_shapes[i, ] <- sf::st_make_valid(species_shapes[i, ])
    }
  }
  
  message("Shape validation complete.")
  return(species_shapes)
}