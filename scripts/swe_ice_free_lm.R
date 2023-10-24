library(tidyverse)
library(lubridate)


bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")

raw_ice_data <- read.csv(file = "../data/analysis_outputs/ice_data_harmonized.csv",
                         stringsAsFactors = FALSE)

View(raw_ice_data)

# Compare Bill's ice free to ours. Quick check looks like our calculations would
# match up.
raw_ice_data %>%
  group_by(location, lake) %>%
  arrange(water_year, .by_group = TRUE) %>%
  mutate(onset = as.Date(x = onset, tz = "PDT"),
         loss = as.Date(x = loss, tz = "PDT"),
         ice_duration_mrb = as.numeric(loss - onset),
         ice_free_mrb = lead(x = onset, n = 1) - loss,
         compare_free = ice_free == ice_free_mrb,
         compare_dur = duration == ice_duration_mrb) %>%
  select(location, lake, water_year, duration, ice_free, contains("mrb"),
         contains("compare"))


# Model ice and SWE
model_data <- bigjoin %>%
  select(park_code, site_code, event_year, SWE_May, ice_free_days) %>%
  unique() %>%
  group_by(park_code, site_code) %>%
  mutate(ice_free_days = scale(ice_free_days)) %>%
  ungroup()
  
ice_model <- lm(formula = ice_free_days ~ SWE_May, data = model_data)

par(mfrow = c(2, 2))
plot(ice_model)

anova(ice_model)

summary(ice_model)

ggplot(data = model_data, aes(x = SWE_May, y = ice_free_days)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
