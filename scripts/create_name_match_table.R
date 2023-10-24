# A quick script to create a map/link table to match common names to site_codes
# and export this as a .rds file to save script text space

library(tidyverse)

# Manually lays out a data frame (tibble):
# Each row in code below is a row in the table.
match_sites <- tribble(
  ~old_name,  ~site_code,
  "Milk",     "498",
  "Heather",  "263",
  "Connie",   "627",
  "LP19",     "LP19",
  "Pallisades", "LH14",
  "Allen",    "LN03",
  "LowerBlum", "LS-07-01",
  "UpperTriplet", "SM-02-02",
  "Ferry",    "138",
  "LH15",     "LH15",
  "Blue",     "LZ35",
  "Deadwood", "LW32",
  "Bowan",    "MR-12-01",
  "EasyRidge", "MC-03-01",
  "LowerEast", "MC-14-02",
  "LowerSilent", "MA-03-01",
  "Gladys",   "106",
  "Crazy",    "426",
  "LaCrosse", "520",
  "Lake Sunup", "623",
  "Milk Lake", "498",
  "Heather Lake", "263",
  "Lake Connie", "627",
  "Lake LP19", "LP19",
  "Upper Palisades Lake", "LH14",
  "Lake Allen", "LN03",
  "Lower Blum Lake", "LS-07-01",
  "Upper Triplet Lake", "SM-02-02",
  "Ferry Lake", "138",
  "Lake LH15", "LH15",
  "Blue Lake", "LZ35",
  "Upper Deadwood Lake", "LW32",
  "Bowan Lake", "MR-12-01",
  "Easy Ridge Lake", "MC-03-01",
  "Lower East Lake", "MC-14-02",
  "Lower Silent Lake", "MA-03-01",
  "Low Silent Lake", "MA-03-01",
  "Gladys Lake", "106",
  "Crazy Lake", "426",
  "Lake La Crosse", "520",
  "Hoh",      "Hoh",
  "SunUp",    "623",
  "Lh15",     "LH15",
  "Upper Palisades", "LH14",
  "Easy Ridge", "MC-03-01",
  "Lower Blum", "LS-07-01",
  "Lower East", "MC-14-02",
  "Lower Silent", "MA-03-01",
  "Upper Triplet",  "SM-02-02",
  "Triplet", "SM-02-02", # Assume same as Upper
  "Silent", "MA-03-01", # Assume same as Lower
  "East", "MC-14-02", # Assume same as Lower
  "Sunup", "623",
  "Lake Sun Up", "623",
  "Blum", "LS-07-01",
  "Hoh Lake", "Hoh",
  "Lake LaCrosse", "520",
  "LS0701", "LS-07-01",
  "MA0301", "MA-03-01",
  "MC0301", "MC-03-01",
  "MC1402", "MC-14-02",
  "MR1201",  "MR-12-01",
  "SM0202", "SM-02-02"
)

saveRDS(object = match_sites,
        file = file.path("..", "data", "name_site_matches.rds"))


