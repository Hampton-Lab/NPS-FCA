
library(tidyverse)
library(viridis)

temp_DO_function1 <- function(x) {(14.7-(0.0017*1))*exp(-0.0225*x)} # function from SH picture
temp_DO_function2 <- function(x) {(14.7-(0.0017*500))*exp(-0.0225*x)} # function from SH picture
temp_DO_function3 <- function(x) {(14.7-(0.0017*1000))*exp(-0.0225*x)} # function from SH picture
temp_DO_function4 <- function(x) {(14.7-(0.0017*1500))*exp(-0.0225*x)} # function from SH picture
temp_DO_function5 <- function(x) {(14.7-(0.0017*2000))*exp(-0.0225*x)} # function from SH picture
temp_DO_function6 <- function(x) {(14.7-(0.0017*2500))*exp(-0.0225*x)} # function from SH picture
temp_DO_function7 <- function(x) {(14.7-(0.0017*3000))*exp(-0.0225*x)} # function from SH picture
temp_DO_function8 <- function(x) {(14.7-(0.0017*3500))*exp(-0.0225*x)} # function from SH picture
temp_DO_function9 <- function(x) {(14.7-(0.0017*4000))*exp(-0.0225*x)} # function from SH picture


data_elevations <- data.frame(x = 3:30,            # Create data for ggplot2
                       values = c(temp_DO_function1(3:30),
                                  temp_DO_function2(3:30),
                                  temp_DO_function3(3:30),
                                  temp_DO_function4(3:30),
                                  temp_DO_function5(3:30),
                                  temp_DO_function6(3:30),
                                  temp_DO_function7(3:30),
                                  temp_DO_function8(3:30),
                                  temp_DO_function9(3:30)),
                       elevation = rep(c(1,
                                         500,
                                         1000,
                                         1500,
                                         2000,
                                         2500,
                                         3000,
                                         3500,
                                         4000), each = 28))


castle_yr_16 <- yr_16 %>% filter(Depth >= 30) %>%
  mutate(week = lubridate::week(Date)) %>%
  group_by(week)%>%
  summarise(temp = mean(Temperature, na.rm = T),
            do = mean(Dissolved.O2.concentration, na.rm = T)) %>%
  mutate(elevation = 1660)

castle_littoral <- read_csv("data/sonde_prep_castle_2015_2019.csv") %>%
  mutate(date = lubridate::ymd_hms(datetime),
         week = lubridate::week(date))%>%
  group_by(week)%>%
  summarise(temp = mean(wtemp, na.rm = T),
            do = mean(do_eq, na.rm = T),
            sd = sd(do_eq, na.rm = T))%>%
  na.omit(.) %>%
  mutate(elevation = 1660)%>%
  mutate(lake = "Castle Lake (3m littoral)")

gl4_littoral <- read_csv("data/water_quality_glv_dm.data.csv") %>%
  filter(location == "INLET") %>%
  filter(local_site == "GL4") %>%
  mutate(date2 = lubridate::mdy(date),
         week = lubridate::week(date2))%>%
  group_by(week)%>%
  summarise(temp = mean(temp, na.rm = T),
            do = mean(DO, na.rm = T),
            sd = sd(DO, na.rm = T))%>%
  na.omit(.) %>%
  mutate(elevation = 3561)%>%
  mutate(lake = "Green Lake 4 inlet")

alb_littoral <- read_csv("data/water_quality_glv_dm.data.csv") %>%
  filter(location == "INLET") %>%
  filter(local_site == "ALB") %>%
  mutate(date2 = lubridate::mdy(date),
         week = lubridate::week(date2))%>%
  group_by(week)%>%
  summarise(temp = mean(temp, na.rm = T),
            do = mean(DO, na.rm = T),
            sd = sd(DO, na.rm = T))%>%
  na.omit(.) %>%
  mutate(elevation = 3359)%>%
  mutate(lake = "Albion Lake Inlet")

sky_depth <- read_csv("data/loch_shy_temp_do.csv") %>%
  filter(lakeID == "SkyPond") %>%
  filter(depth == 6.5) %>%
  mutate(elevation = 3322)%>%
  mutate(lake = "Sky Pond (6.5m pelagic)")

loch_depth <- read_csv("data/loch_shy_temp_do.csv") %>%
  filter(lakeID == "TheLoch") %>%
  filter(depth == 4.5) %>%
  mutate(elevation = 3048)%>%
  mutate(lake = "Loch Vale (4.5m pelagic)")

shallow_parks <- env_dat_yr %>% filter(Depth_max <= 6.5) %>%
  select(site_code, Depth_max, Elevation_m, BotTemp, DO_below2m, event_year) %>%
  na.omit(.) %>%
  arrange(site_code, event_year)

bowan <- shallow_parks %>% filter(site_code == "Bowan") %>%
  rename(do = DO_below2m, temp = BotTemp, elevation = Elevation_m) %>%
  mutate(lake = "Bowan (3.6m pelagic)")

deadwood <- shallow_parks %>% filter(site_code == "Deadwood")%>%
  rename(do = DO_below2m, temp = BotTemp, elevation = Elevation_m) %>%
  mutate(lake = "Deadwood (3.8m pelagic)")

easyridge <- shallow_parks %>% filter(site_code == "EasyRidge")%>%
  rename(do = DO_below2m, temp = BotTemp, elevation = Elevation_m) %>%
  mutate(lake = "Easy Ridge (3.6m pelagic)")

gladys <- shallow_parks %>% filter(site_code == "Gladys")%>%
  rename(do = DO_below2m, temp = BotTemp, elevation = Elevation_m) %>%
  mutate(lake = "Glady's (4.2m pelagic)")

sunup <- shallow_parks %>% filter(site_code == "Sunup")%>%
  rename(do = DO_below2m, temp = BotTemp, elevation = Elevation_m) %>%
  mutate(lake = "Sunup (6m pelagic)")

triplet <- shallow_parks %>% filter(site_code == "Triplet")%>%
  rename(do = DO_below2m, temp = BotTemp, elevation = Elevation_m) %>%
  mutate(lake = "Triplet (3.9m pelagic)")


data_eval <- ggplot(data_elevations,                                   # Draw ggplot2 plot
       aes(x, values, group = elevation, col = elevation)) +
  geom_line()+
  # geom_point(data = triplet, aes(x = temp, y = do, group = elevation, color = elevation), pch = 19, size = 3)+
  #   geom_path(data = triplet, aes(x = temp, y = do, group = elevation), color = "grey50",
  #             arrow = arrow(length = unit(0.3, "cm")))+
  geom_point(data = castle_littoral, aes(x = temp, y = do, group = lake, fill = lake), pch = 21, size = 3)+
  geom_path(data = castle_littoral, aes(x = temp, y = do, group = elevation), color = "grey50",
            arrow = arrow(length = unit(0.3, "cm")))+
  geom_point(data = gl4_littoral, aes(x = temp, y = do, group = lake, fill = lake), pch = 21, size = 3)+
  geom_path(data = gl4_littoral, aes(x = temp, y = do, group = elevation), color = "grey50",
            arrow = arrow(length = unit(0.3, "cm")))+
  # geom_point(data = sunup, aes(x = temp, y = do, group = lake, fill = lake), pch = 21, size = 3)+
  # geom_path(data = sunup, aes(x = temp, y = do, group = elevation), color = "grey50",
  #           arrow = arrow(length = unit(0.3, "cm")))+
  geom_point(data = loch_depth, aes(x = temp, y = do, group = lake, fill = lake), pch = 21, size = 3)+
  geom_path(data = loch_depth, aes(x = temp, y = do, group = elevation), color = "grey50",
            arrow = arrow(length = unit(0.3, "cm")))+
  geom_point(data = sky_depth, aes(x = temp, y = do, group = lake, fill = lake), pch = 21, size = 3)+
  geom_path(data = sky_depth, aes(x = temp, y = do, group = elevation), color = "grey50",
            arrow = arrow(length = unit(0.3, "cm")))+
  # geom_point(data = gladys, aes(x = temp, y = do, group = lake, fill = lake), pch = 21, size = 3)+
  # geom_path(data = gladys, aes(x = temp, y = do, group = elevation), color = "grey50",
  #           arrow = arrow(length = unit(0.3, "cm")))+
  # geom_point(data = easyridge, aes(x = temp, y = do, group = lake, fill = lake), pch = 21, size = 3)+
  # geom_path(data = easyridge, aes(x = temp, y = do, group = elevation), color = "grey50",
  #           arrow = arrow(length = unit(0.3, "cm")))+
  # geom_point(data = deadwood, aes(x = temp, y = do, group = lake, fill = lake), pch = 21, size = 3)+
  # geom_path(data = deadwood, aes(x = temp, y = do, group = elevation), color = "grey50",
  #           arrow = arrow(length = unit(0.3, "cm")))+
  scale_color_viridis(discrete = F, option = "D")+
  scale_fill_viridis(discrete = T, option = "C")+
  theme_light()+
  labs(x = "Bottom Water Temperature (C)", y = "Bottom Water Dissolved Oxygen (mg/L)")

ggsave(data_eval, path = ".",
       filename = "./lake_littoral_plot.jpg",
       width = 8, height = 6, device='jpg', dpi=1000)

