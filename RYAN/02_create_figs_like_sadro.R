for(s in 1:length(lakes)){

  t_max <- list.files(path, pattern = paste0("_",lakes[s],"_")) %>%
    map(~read_csv(paste0(path,.), col_types = cols(), col_names = FALSE, id = "file_name", skip = 4))%>%
    data.table::rbindlist(fill = T)%>%
    rename(date_time = X1,
           temp_C = X2)%>%
    select(-X3,-X4,-X5)%>%
    mutate(file_name = case_when(
      file_name == paste0("./data/temp_time_series/AirTemp_",lakes[s],"_POR.csv") ~ "AirTemp",
      file_name == paste0("./data/temp_time_series/BottomTemp_",lakes[s],"_POR.csv") ~ "BottomTemp",
      file_name == paste0("./data/temp_time_series/MidTemp_",lakes[s],"_POR.csv") ~ "MidTemp",
      file_name == paste0("./data/temp_time_series/SurfaceTemp_",lakes[s],"_POR.csv") ~ "SurfTemp",
      TRUE ~ NA_character_))%>%
    mutate(date_time = lubridate::mdy_hms(date_time))%>%
    filter(file_name=="AirTemp")%>%
    mutate(date = lubridate::date(date_time),
           month = lubridate::month(date),
           year = lubridate::year(date))%>%
    #filter(month %in% c("7","8","9"))%>%
    group_by(year)%>%
    summarize(yearly_temp_max = mean(temp_C, na.rm = T),
              yearly_temp_sd = sd(temp_C, na.rm = T))%>%
    mutate(site_code = lakes[s])

  lake_temp[[s]] <- t_max

}

air_temps = do.call(rbind, lake_temp) %>%
  filter(year >= "2009")%>%
  mutate(year = as.numeric(year))

mean_air_temps <- air_temps %>%
  group_by(year, site_code)%>%
  summarize(mean = mean(yearly_temp_max, na.rm = T),
            sd = mean(yearly_temp_max))


lake_air = env_dat_yr %>%
  group_by(event_year, site_code, park_code)%>%
  summarize(SWE_may_mean = mean(SWE_May_snotel, na.rm = T),
            SWE_may_sd = mean(SWE_May_snotel, na.rm = T))%>%
  filter(event_year >= "2009")%>%
  filter(event_year< "2019")%>%
  rename(year = event_year)%>%
  mutate(year = as.numeric(year)) %>% left_join(., mean_air_temps, by = c("year"))%>%
  group_by(year)%>%
  summarise(mean_t = mean(mean,  na.rm = T),
            sd = sd(mean, na.rm = T))


air_temps <- ggplot(lake_air, aes(year, mean_t))+
  geom_errorbar(data = lake_air, aes(x=as.numeric(year), ymin = mean_t - sd,
                                     ymax = mean_t + sd), width=.2)+
  geom_point(size = 5, color = "black", pch = 21, fill = "grey")+
  geom_smooth(method = "lm", color = "black", se = F)+
  theme_classic()+
  ylab("Air temp. above lakes (C)")+
  xlab("")+
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 20))

sens.slope(lake_air$mean_t)

SWE_anom <- lake_swe %>% ungroup(.) %>%
  ungroup(.)%>%
  mutate(anom = SWE_may_mean - 150)%>%
  group_by(year)%>%
  summarize_all(funs(mean), na.rm = T)%>%
  mutate(SWE_extreme = ifelse(SWE_may_mean <= 119.1,"low","norm"))%>%
  mutate(SWE_extreme = ifelse(SWE_may_mean >= 194.3,"high",SWE_extreme))%>%
  select(year,SWE_extreme,anom)%>%
  na.omit(.)%>%
  ggplot(., aes(as.character(year), anom))+
  geom_col(aes(fill = SWE_extreme), color = "black")+
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "SWE extreme") +
  ylim(c(-130, 130))+
  xlab("")+
  ylab("SWE anomoly (cm)")+
  theme_classic()+
  theme(legend.position = "top", axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 20))

lake_temps <- mean_lake_temps %>%
  mutate(SWE_extreme = ifelse(mean <= 11.383,"cold","norm"))%>%
  mutate(SWE_extreme = ifelse(mean >= 14.602,"hot",SWE_extreme))%>%
  ggplot(., aes(year,mean))+
  geom_point(aes(fill = SWE_extreme),color = "black", pch = 21, size = 5)+
  geom_errorbar(aes(x=as.numeric(year), ymin = mean - sd,
                                     ymax = mean + sd, color = SWE_extreme), width=.2)+
  geom_smooth(aes(group = SWE_extreme, color = SWE_extreme),method = "lm", se = F)+
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Temperature extreme")+
  scale_color_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Temperature extreme")+
  xlab("Year")+
  ylab("Mean water-column temp. (C)")+
  theme_classic()+
  theme(legend.position = "top", axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20))

figure <- air_temps/SWE_anom/lake_temps
ggsave("./figures/temp_swe_temp_FIG1.jpg", width = 12, height = 12, units = "in", dpi = 1000)




