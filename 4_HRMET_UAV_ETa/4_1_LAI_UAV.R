library(terra)
library(LAIr)


# Reading the RGBneIR image to calculate NDVI
input_raster <- rast("Data/Multispectral_Imagery_3Aug2022/LTARgatesburg_2022Aug03_AltumSR_RGBnirRE_WGS84_UTM17N.tif")

# plot true color
# Extract RGB bands (assuming Band 1: Red, Band 2: Green, Band 3: Blue)
true_color <- input_raster[[c(1, 2, 3)]]

# Plotting the true color image
plotRGB(true_color, r = 1, g = 2, b = 3, stretch = "lin", main = "True Color Composite")

# Calculate NDVI from the raster
red <- input_raster[[1]]
nir <- input_raster[[4]]

ndvi <- (nir - red) / (nir + red)

# Plot NDVI
plot(ndvi, main = "NDVI")
writeRaster(ndvi, "ndvi_20220803_complete.tif", overwrite = TRUE)


# Path to the large NDVI raster
ndvi_path <- "ndvi_20220803_complete.tif"

# Open the raster (without loading it entirely)
ndvi_raster <- rast(ndvi_path)
plot(ndvi_raster, main = "NDVI")

# Define chunk size
chunk_size <- 1000  # Number of rows per chunk

# Temporary directory for chunk files
temp_dir <- tempdir()

# Create a list to store processed chunks
chunk_files <- list()

# Process in chunks (Estinate LAI in chuncks)
for (start_row in seq(1, nrow(ndvi_raster), by = chunk_size)) {
  
  end_row <- min(start_row + chunk_size - 1, nrow(ndvi_raster))
  
  # Get y-coordinates for the chunk
  ymin <- ymin(ndvi_raster) + (nrow(ndvi_raster) - end_row) * yres(ndvi_raster)
  ymax <- ymin(ndvi_raster) + (nrow(ndvi_raster) - start_row + 1) * yres(ndvi_raster)
  
  # Get x-coordinates for the entire raster
  xmin <- xmin(ndvi_raster)
  xmax <- xmax(ndvi_raster)
  
  # Create extent using spatial coordinates
  chunk_extent <- ext(xmin, xmax, ymin, ymax)
  
  # Crop the raster using the calculated extent
  chunk <- crop(ndvi_raster, chunk_extent)
  
  # Apply the NDVI to LAI conversion to the chunk
  lai_chunk <- NDVI2LAI(chunk, category = 'Crop', type='Summer', name = 'Maize',
                        sensor = 'UAV', platform='Airborne', resolution='Very-High')
  
  # Save the processed chunk to a temporary file
  chunk_filename <- file.path(temp_dir, paste0("lai_chunk_", start_row, ".tif"))
  writeRaster(lai_chunk, chunk_filename, overwrite = TRUE)
  
  # Store the filename
  chunk_files[[length(chunk_files) + 1]] <- chunk_filename
  
  # Free memory
  rm(chunk, lai_chunk)
  gc()
  
  message("Processed chunk from row ", start_row, " to ", end_row)
}

# Mosaic all processed chunks
lai_chunks <- lapply(chunk_files, rast)

#mean_na <- function(x) {
  #if (all(is.na(x))) return(NA)
  #mean(x, na.rm = TRUE)
#}

lai_mosaic <- do.call(mosaic, c(lai_chunks, fun = mean))

plot(lai_mosaic)

# Saving all generated LAI

for (i in 1:nlyr(lai_mosaic)) {
  # Extract the layer
  layer <- lai_mosaic[[i]]
  
  # Get the original layer name
  layer_name <- names(lai_mosaic)[i]
  
  # Create the filename using the layer name
  layer_filename <- file.path(paste0(layer_name, ".tif"))
  
  # Save the layer
  writeRaster(layer, layer_filename, overwrite = TRUE)
  
  message("Saved layer '", layer_name, "' as: ", layer_filename)
}
