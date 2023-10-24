# This script takes two files of ice-on/ice-off data and combines them into one,
# with some cleaning

# Load packages
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)


# Load the datasets -------------------------------------------------------

ice_original <- read_excel(path = file.path("..",
                                            "data",
                                            "ice_spreadsheets_from_WB_2019May3",
                                            "IceDurationSummary.xlsx"),
                           sheet = "IceIn_Out_Dates")

ice_update <- read_excel(path = file.path("..",
                                          "data",
                                          "ice_spreadsheets_from_WB_2019May3",
                                          "Ice_Loss_update_071019.xlsx"),
                         sheet = 1)


# Wrangle and harmonize ---------------------------------------------------

# Fix some Excel date issues
ice_original <-  ice_original %>% 
  mutate(Onset_new = if_else(condition = stringr::str_length(Onset) > 5,
                             true = as.Date(Onset, format = "%m/%d/%Y"),
                             false = excel_numeric_to_date(as.integer(Onset))),
         Loss_new = if_else(condition = stringr::str_length(Loss) > 5,
                            true = as.Date(Loss, format = "%m/%d/%Y"),
                            false = excel_numeric_to_date(as.integer(Loss)))) %>%
  clean_names() %>%
  select(-c(onset, loss)) %>%
  select(lake:water_year, onset = onset_new, loss = loss_new,
         duration:ice_out_day_numerical)

ice_update <- ice_update %>%
  mutate(Onset_new = if_else(condition = stringr::str_length(Onset) > 5,
                             true = as.Date(Onset, format = "%m/%d/%Y"),
                             false = excel_numeric_to_date(as.integer(Onset))),
         Loss_new = case_when(
           # Text notes are NAs (case_when requires all same type, i.e. as.Date)
           grepl(pattern = "[a-z]", x = Loss, ignore.case = TRUE) ~ as.Date(NA),
           # Reformat any decimal numbers
           grepl(pattern = "\\.", x = Loss) & !grepl(pattern = "[a-z]", x = Loss) ~
             excel_numeric_to_date(as.integer(Loss)),
           # NAs remain. Must conform to Date type
           is.na(Loss) ~ as.Date(NA) ,
           # Reformat dates that made it through
           grepl(pattern = "/", x = Loss) ~ as.Date(Loss, format = "%m/%d/%Y"),
           # Reformat Excel dates
           TRUE ~ excel_numeric_to_date(as.integer(Loss)))) %>%
  clean_names() %>%
  select(-c(onset, loss)) %>%
  select(lake:water_year, onset = onset_new, loss = loss_new)


# CAN'T left_join ice_orig to ice_update bc update doesn't have OLYM
# CAN'T case_when, because not same number of rows
# So...can an anti_join or semi_join contribute?

# Check to make sure all "original" ice-on == "update" ice-on
left_join(x = ice_original, y = ice_update,
          by = c("lake", "location", "water_year")) %>%
  mutate(iceon_equal = case_when(
    onset.x == onset.y ~ "Equal",
    (!is.na(onset.x) & !is.na(onset.y)) & (onset.x != onset.y) ~ "Not equal",
    is.na(onset.x) & !is.na(onset.y) ~ "Original is NA",
    !is.na(onset.x) & is.na(onset.y) ~ "Update is NA",
    is.na(onset.x) & is.na(onset.y) ~ "Both NA")) %>%
  group_by(iceon_equal) %>%
  count()

# # A tibble: 3 x 2
# # Groups:   iceon_equal [3]
# iceon_equal      n
# <chr>        <int>
# 1 Both NA         21
# 2 Equal           69
# 3 Update is NA    94

# Therefore, OK to just use original value going forward for ice-on

# What are the patterns with ice-off?
left_join(x = ice_original, y = ice_update,
          by = c("lake", "location", "water_year")) %>%
  mutate(iceoff_equal = case_when(
    loss.x == loss.y ~ "Equal",
    (!is.na(loss.x) & !is.na(loss.y)) & (loss.x != loss.y) ~ "Not equal",
    is.na(loss.x) & !is.na(loss.y) ~ "Original is NA",
    !is.na(loss.x) & is.na(loss.y) ~ "Update is NA",
    is.na(loss.x) & is.na(loss.y) ~ "Both NA")) %>%
  group_by(iceoff_equal) %>%
  count()

# # A tibble: 4 x 2
# # Groups:   iceoff_equal [4]
# iceoff_equal       n
# <chr>          <int>
# 1 Both NA           13
# 2 Equal             69
# 3 Original is NA    12
# 4 Update is NA      90

# NOTE:
# On 2019-09-25 MRB switched to full_join because, e.g. 2011 Lower Blum data
# weren't being added to the dataset with left_join.

# Now perform the left_join and ice-off value replacement
ice_harmonized <- full_join(x = ice_original, y = ice_update,
                            by = c("lake", "location", "water_year")) %>%
  mutate(onset = onset.x,
         loss = case_when(
           loss.x == loss.y ~ loss.x,
           is.na(loss.x) & !is.na(loss.y) ~ loss.y,
           !is.na(loss.x) & is.na(loss.y) ~ loss.x),
         ice_out_doy = yday(x = loss),
         ice_in_doy = yday(x = onset)) %>%
  select(-c(onset.x, onset.y, loss.x, loss.y)) %>%
  clean_names()

# Export
write.csv(x = ice_harmonized,
          file = "../data/analysis_outputs/ice_data_harmonized.csv",
          row.names = FALSE)




