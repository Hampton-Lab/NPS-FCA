# Quick script to compare SNODAS SWE to SNOTEL SWE

library(tidyverse)
library(corrplot)
library(rpart)
library(partykit)


bigjoin <- read_rds("../data/analysis_outputs/bigjoin.rds")

bigjoin %>%
  select(park_code, site_code, event_year, event_month,
         SWE_May, SWE_Apr, SWE_May_snotel, SWE_Apr_snotel) %>%
  unique() %>%
  nrow()

# Pull out the SNODAS and SNOTEL data
snotel_snodas <- bigjoin %>%
  select(park_code, site_code, event_year,
         SWE_May_snodas = SWE_May, SWE_Apr_snodas = SWE_Apr,
         SWE_May_snotel, SWE_Apr_snotel) %>%
  unique()

# Pull out explanatory watershed vars of interest
expl_vars <- bigjoin %>%
  select(site_code, park_code, Aspect, Elevation_m, Watershed_area_to_lake_area_ratio,
         Surface_area_ha, Watershed_area_ha, Depth_mean_m, forest, shrub,
         meadow, barren, snowice) %>%
  unique()

# Quick visual of SNOTEL ~ SNODAS with a 1:1 line
snotel_snodas %>%
  ggplot() +
  geom_point(aes(x = SWE_May_snotel, y = SWE_May_snodas)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(. ~ park_code) +
  xlim(c(0, 350)) +
  ylim(c(0, 350))

# Get slopes by lake
sno_slopes <- split(x = snotel_snodas, f = snotel_snodas$site_code) %>%
  map_df(.x = .,
         .f = ~ 
           
           {
             # Get slope
             lm_coef <- coef(lm(formula = SWE_May_snodas ~ SWE_May_snotel,
                                data = .x))["SWE_May_snotel"]
             
             # Get site
             site <- unique(.x$site_code)
             
             # Pair for output
             data.frame(lm_coef, site)
           }
         
  )

# Join slopes to watershed data
sno_slope_match <- inner_join(x = sno_slopes,
                              y = expl_vars,
                              by = c("site" = "site_code"))

# Correlation plot
corrplot(cor(na.omit(select(sno_slope_match, -site, -park_code))))

# Run simple lm
slope_model <- lm(formula = lm_coef ~ Depth_mean_m +
                    Surface_area_ha + forest,
                  data = sno_slope_match)

summary(slope_model)


# Regression tree
slope_tree <- rpart(formula = lm_coef ~ Aspect + Elevation_m +
                       Watershed_area_to_lake_area_ratio + Surface_area_ha +
                       Watershed_area_ha + Depth_mean_m + forest + shrub + meadow +
                       barren + snowice,
                     data = sno_slope_match, minsplit = 10)

plot(x = as.party(slope_tree),
     main = "SNODAS ~ SNOTEL slopes",
     terminal_panel = node_boxplot)

plotcp(slope_tree)


