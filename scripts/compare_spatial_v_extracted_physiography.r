library(tidyverse)


# 1. Load datasets needed -------------------------------------------------

lakes_shp <- read.csv("../data/analysis_outputs/lake_shp_attributes.csv",
                      stringsAsFactors = F)

watershed_shp <- read.csv("../data/analysis_outputs/watershed_shp_attributes.csv",
                          stringsAsFactors = F)

pdf_data <- read_excel(path = "../data/analysis_outputs/study-site-tables.xlsx",
                       sheet = 1)

match_table <- read_rds(path = "../data/name_site_matches.rds")


# 2. Standardize naming across all, then join -----------------------------

pdf_data <- left_join(x = pdf_data, y = match_table, by = c("Lake" = "old_name"))

unique(lakes_shp$NAME)
unique(watershed_shp$Name)
lakes_gis_join <- left_join(x = lakes_shp, y = watershed_shp,
                            by = c("NAME" = "Name"),
                            suffix = c("_lakeGIS", "_watershedGIS"))


lakes_gis_join <- left_join(x = lakes_gis_join, y = match_table,
                            by = c("NAME" =  "old_name"))


lakes_gis_pdf_join <- inner_join(x = lakes_gis_join, y = pdf_data,
                                 by = "site_code") %>%
  # New comparison columns
  mutate(lake_area_diff = Area_ha_lakeGIS - Surface_area_ha,
         watershed_area_diff = Area_ha_watershedGIS - Watershed_area_ha) %>%
  # Keep what's needed
  select(lake_shapefile_name = NAME, site_code, SOURCE, Park_code,
         Area_ha_lakeGIS, Surface_area_ha_PDF = Surface_area_ha, lake_area_diff,
         Area_ha_watershedGIS, Watershed_area_ha_PDF = Watershed_area_ha,
         watershed_area_diff)


# 3. Export output --------------------------------------------------------

write.csv(x = lakes_gis_pdf_join,
          file = "../data/analysis_outputs/compare_spatial_v_extracted.csv",
          row.names = FALSE)
