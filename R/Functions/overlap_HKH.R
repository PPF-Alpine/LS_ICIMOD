overlap_mountains_and_alpinebiome <- function(species_shapes, mountain_shapes, alpine_biome) {
  tryCatch({
    ordered_data <- order_by_mountain_range(mountain_shapes, alpine_biome)
  }, error = function(e) {
    stop("Error in order_by_mountain_range: ", e$message)
  })
  
  tryCatch({
    valid_species_shapes <- validate_shapes(species_shapes)
  }, error = function(e) {
    stop("Error in validate_shapes: ", e$message)
  })
  
  tryCatch({
    mountain_bboxes_sf <- create_bounding_boxes(ordered_data$mountain_shapes)
  }, error = function(e) {
    stop("Error in create_bounding_boxes: ", e$message)
  })
  
  total_species <- nrow(valid_species_shapes)
  processed_results <- list()
  not_processed_species <- list()
  
  for (i in seq_len(total_species)) {
    species <- valid_species_shapes[i, ]
    
    processing_result <- calculate_overlaps(species, ordered_data$mountain_shapes, ordered_data$alpine_biome, mountain_bboxes_sf)
    
    if (!is.null(processing_result$results) && !processing_result$error) {
      processed_results[[length(processed_results) + 1]] <- processing_result$results
    } else if (processing_result$error) {
      # Add to not_processed_species only if an actual error occurred
      not_processed_species[[length(not_processed_species) + 1]] <- species$sciname
    }
    
    message(sprintf("Yay! %d of %d species are done", i, total_species))
  }
  
  processed_df <- do.call(rbind, processed_results)
  not_processed_df <- data.frame(sciname = unlist(not_processed_species))
  
  return(list(processed = processed_df, not_processed = not_processed_df))
}