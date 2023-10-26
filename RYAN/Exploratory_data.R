
slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}


fully_aquatic <- macro_join %>% left_join(., env_zoop_data,
                                          by = c("park_code","site_code","event_year","solar_jas","Elevation_m","Depth_max","lon","lat" ,
                                                 "Ca","Chlorophyll","DOC", "ice_out_doy","Total N","Total P",
                                                 "fish","lake_temp","stability","delta_temp"))%>%
  select(-Plecoptera, -Coleoptera, -Diptera, -Ephemeroptera, -Hemiptera, -Anthoathecatae, -Megaloptera,
         -Odonata, -Plecoptera, -Trichoptera, -Poridera)%>%
  melt(., id.vars = c("park_code","site_code","event_year","solar_jas","Elevation_m","Depth_max","lon","lat" ,
                      "Ca","Chlorophyll","DOC", "ice_out_doy","Total N","Total P",
                      "fish","lake_temp","stability","delta_temp"))%>%
  mutate(value = as.numeric(value))%>%
  group_by(event_year, site_code, park_code)%>%
  summarize_all(funs(mean), na.rm = T)%>%
  mutate(type = "fully")

ggplot(fully_aquatic, aes(lake_temp, value, group = site_code, color = park_code))+geom_point()+
  geom_smooth(method = "lm", se = F)+
  ylab("Fully Aquatic")+
  xlab("Temperature")+
  theme_classic()

ggplot(env_zoop_data, aes(lake_temp, Chlorophyll, group = park_code, color = park_code))+geom_point()+
  geom_smooth(method = "lm", se = T)+
  ylab("chla")+
  xlab("Temperature")+
  theme_classic()

library(ggpubr)

fully_slopes <- fully_aquatic %>%
  group_by(site_code) %>%
  mutate(slope = slope(lake_temp, value))%>%
  ggplot(aes(x = park_code, slope))+
  geom_boxplot()+
  geom_jitter(width = 0.05)+
  stat_compare_means(label = "p.signif", method = "t.test",label.x = 1.5, size = 10, paired = F)+
  theme_classic()

semi_aquatic <- macro_join %>% left_join(., env_zoop_data,
                                         by = c("park_code","site_code","event_year","solar_jas","Elevation_m","Depth_max","lon","lat" ,
                                                "Ca","Chlorophyll","DOC", "ice_out_doy","Total N","Total P",
                                                "fish","lake_temp","stability","delta_temp"))%>%
  select(-Acari, -Amphipoda, -Basommatophora, -COPE, -RAP, -MICRO, -CLAD,
         -Diplostraca, -Hirudinida,
         -Isopoda, -Nmorpha, -Ntoda, -Ntopda,
         -Oligo, -Ostra, -Platy,
         -Veneroida, -Anthoathecatae, -Poridera)%>%
  melt(., id.vars = c("park_code","site_code","event_year","solar_jas","Elevation_m","Depth_max","lon","lat" ,
                      "Ca","Chlorophyll","DOC", "ice_out_doy","Total N","Total P",
                      "fish","lake_temp","stability","delta_temp"))%>%
  mutate(value = as.numeric(value))%>%
  group_by(event_year, site_code, park_code)%>%
  summarize_all(funs(mean), na.rm = T)%>%
  mutate(type = "semi")

ggplot(semi_aquatic, aes(delta_temp, value, group = site_code, color = park_code))+geom_point()+
  geom_smooth(method = "lm", se = F)+
  ylab("Semi Aquatic")+
  xlab("Temperature")+
  theme_classic()

semi_slopes <- semi_aquatic %>%
  group_by(site_code) %>%
  mutate(slope = slope(lake_temp, value))%>%
  ggplot(aes(x = park_code, slope))+
  geom_boxplot()+
  geom_jitter(width = 0.05)+
  stat_compare_means(label = "p.signif", method = "t.test",label.x = 1.5, size = 10, paired = F)+
  theme_classic()



both <- bind_rows(fully_aquatic, semi_aquatic)%>%
  select(-variable, -site_code)%>%
  arrange(park_code, event_year)%>%
  group_by(park_code, event_year, type)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_value = as.numeric(lag(value)))%>%
  arrange(park_code, event_year)%>%
  ungroup(.)%>%
  select(-site_code)%>%
  mutate_at(vars(-park_code, -event_year),
            funs(imputeTS::na_interpolation(., option = "spline")))%>%
  ggplot(., aes(x = as.numeric(event_year), y = value, group = type, color = type))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~park_code)+
  theme_classic()

#
#
#
# sessile_aquatic <- macro_join %>% left_join(., env_zoop_data,
#                                          by = c("park_code","site_code","event_year","solar_jas","Elevation_m","Depth_max","lon","lat" ,
#                                                 "Ca","Chlorophyll","DOC", "ice_out_doy","Total N","Total P",
#                                                 "fish","lake_temp","stability","delta_temp"))%>%
#   select(-Acari, -Amphipoda, -Basommatophora, -COPE, -RAP, -MICRO, -CLAD,
#          -Coleoptera, -Diplostraca, -Diptera, -Ephemeroptera, -Hemiptera, -Hirudinida,
#          -Isopoda, -Megaloptera, -Nmorpha, -Ntoda, -Ntopda, -Odonata,
#          -Oligo, -Ostra, -Platy, -Plecoptera, -Trichoptera,
#          -Veneroida)%>%
#   melt(., id.vars = c("park_code","site_code","event_year","solar_jas","Elevation_m","Depth_max","lon","lat" ,
#                       "Ca","Chlorophyll","DOC", "ice_out_doy","Total N","Total P",
#                       "fish","lake_temp","stability","delta_temp"))%>%
#   mutate(value = as.numeric(value))%>%
#   group_by(event_year, site_code, park_code)%>%
#   summarize_all(funs(mean), na.rm = T)
#
# ggplot(sessile_aquatic, aes(lake_temp, value, color = park_code))+geom_point()+
#   geom_smooth(method = "lm")+
#   ylab("Fully Aquatic")+
#   xlab("Temperature")+
#   theme_classic()
