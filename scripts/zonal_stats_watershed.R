library(tidyverse)
library(raster)
library(sf)


# 1. Load data ------------------------------------------------------------

# Import the watershed polygons
watersheds <- st_read(dsn = "../data/NCCN_MtnLakes_ACa02_FCA.gdb",
                      layer = "Lake_Watersheds")

# Note: Can check layers with st_layers()

watersheds_crs <- st_crs(watersheds)$proj4string

# Get paths of NADP raster files
deposition_paths <- dir(path = "../data/deposition_data",
                        full.names = TRUE,
                        recursive = TRUE,
                        pattern = ".tif$")

# Get names of each file from paths
deposition_names <- deposition_paths %>%
  gsub(pattern = "../data/deposition_data/", replacement = "") %>%
  gsub(pattern = "/.+", replacement = "")

# Import all raster files and reproject to match 
deposition_data <- map2(.x = deposition_paths,
                        .y = deposition_names,
                        .f = ~ assign(x = .y,
                                      value = raster(x = .x)) %>%
                          projectRaster(from = .,
                                        crs = watersheds_crs))

# Confirm identical projections
st_crs(watersheds)
crs(deposition_data[[1]])

# Is this the projection we want? What about the method for transforming?
# Default method is bilinear

# Check coverage:
ggplot(data = watersheds) +
  geom_sf()

test_spdf <- as(deposition_data[[1]], "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

ggplot() +  
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), alpha = 0.8) +
  geom_sf(data = watersheds, color = "red")


# 2. Perform zonal stats --------------------------------------------------

# For each raster (i.e., annually), extract a summary stat
deposition_mean <- map2_df(.x = deposition_data,
                           .y = deposition_names,
                           .f = ~ extract(x = .x,
                                          y = watersheds,
                                          fun = mean,
                                          na.rm = TRUE) %>%
                             data.frame() %>%
                             mutate(lake = watersheds$Name,
                                    variable = .y,
                                    measure = "mean") %>%
                             rename(value = 1))

deposition_max <- map2_df(.x = deposition_data,
                          .y = deposition_names,
                          .f = ~ extract(x = .x,
                                         y = watersheds,
                                         fun = max,
                                         na.rm = TRUE) %>%
                            data.frame() %>%
                            mutate(lake = watersheds$Name,
                                   variable = .y,
                                   measure = "max") %>%
                            rename(value = 1))

deposition_min <- map2_df(.x = deposition_data,
                          .y = deposition_names,
                          .f = ~ extract(x = .x,
                                         y = watersheds,
                                         fun = min,
                                         na.rm = TRUE) %>%
                            data.frame() %>%
                            mutate(lake = watersheds$Name,
                                   variable = .y,
                                   measure = "min") %>%
                            rename(value = 1))

deposition_median <- map2_df(.x = deposition_data,
                             .y = deposition_names,
                             .f = ~ extract(x = .x,
                                            y = watersheds,
                                            fun = median,
                                            na.rm = TRUE) %>%
                               data.frame() %>%
                               mutate(lake = watersheds$Name,
                                      variable = .y,
                                      measure = "median") %>%
                               rename(value = 1))

deposition_sd <- map2_df(.x = deposition_data,
                         .y = deposition_names,
                         .f = ~ extract(x = .x,
                                        y = watersheds,
                                        fun = sd,
                                        na.rm = TRUE) %>%
                           data.frame() %>%
                           mutate(lake = watersheds$Name,
                                  variable = .y,
                                  measure = "sd") %>%
                           rename(value = 1))

deposition_outputs <- bind_rows(deposition_mean, deposition_max, deposition_min,
                                deposition_median, deposition_sd)

# Export the output
write.csv(x = deposition_outputs,
          file = "../data/analysis_outputs/deposition_zonal_stats.csv",
          row.names = FALSE)
