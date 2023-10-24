library(tidyverse)

bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")


# Format dataset using high and low 
bigjoin_scale <- bigjoin %>%
  filter(variable %in% c("ProfTemp_top2m", "DO_top2m", "Chlorophyll", 
                         "secchi_value_m", "pH_top2m", "SpCond_top2m",
                         "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>%
  select(park_code, site_code, event_year, variable, value) %>%
  group_by(site_code, variable) %>%
  mutate(value = scale(x = value))


ggplot(data = bigjoin_scale) +
  geom_boxplot(aes(x = variable, y = value), alpha = 0.5) +
  geom_jitter(data = filter(bigjoin_scale,
                            event_year %in% c(2011, 2015)),
              aes(x = variable, y = value, color = as.factor(event_year))) + 
  theme_bw() +
  ylim(c(-4, 4))

