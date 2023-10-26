# BASIC PLOTTING

viz_dat <- vis_dat(env_zoop_data)
viz_dat

viz_dat <- vis_dat(macro_join)
viz_dat


ggplot(zoop_fishless, aes(SWE_May_snotel, lake_temp, color = park_code, fill = park_code))+
  geom_point(pch = 21, size = 2.5, alpha = 0.6)+
  geom_smooth(method = "lm")+
  ylab("Lake Temperature (C)")+
  xlab("Ice Out DOY")+
  theme_classic()

ggplot(env_zoop_data, aes(as.numeric(event_year), lake_temp, color = park_code, fill = park_code))+
  geom_point(pch = 21, size = 2.5, alpha = 0.6)+
  geom_smooth(method = "lm")+
  ylab("Lake Temperature (C)")+
  xlab("Year")+
  theme_classic()


zoop_fishless %>% filter(park_code != "MORA")%>%
ggplot(., aes(as.numeric(event_year), SWE_May_snotel, color = park_code, fill = park_code))+
  geom_point(pch = 21, size = 2.5, alpha = 0.6)+
  geom_smooth(method = "lm")+
  ylab("YEAR")+
  xlab("SWE_May_snotel (Basin Average)")+
  theme_classic()

zoop_fishless %>% filter(park_code != "MORA")%>%
  ggplot(., aes(SWE_May_snotel, RAP, color = park_code, fill = park_code))+
  geom_point(pch = 21, size = 2.5, alpha = 0.6)+
  geom_smooth(method = "lm")+
  ylab("log(COPE+1)")+
  xlab("SWE (mm)")+
  theme_classic()
