# Analyze Time Series of Temperature in the National Park Lakes
se <- function(x) sd(x) / sqrt(length(x))
normalize <- function(x){(x-min(x))/(max(x)-min(x))}
standard_error <- function(x) {sd(x) / sqrt(length(x))}
percent_change <- function(x) {((x - lead(x))/(x))*100}
zscore <- function(x) {(x - mean(x))/sd(x)}
slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}

path = "./data/temp_time_series/"

lakes <- c("Connie", "Crazy", "Ferry", "Gladys", "Heather", "LaCrosse", "Milk", "Sunup",
           "Blue", "Allen", "Bowan", "Deadwood", "EasyRidge", "LH15", "LowerBlum", "LowerEast",
           "LowerSilent", "LP19", "Pallisades", "UpperTriplet")

lake_temp <- list()

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
  filter(file_name!="AirTemp")%>%
  mutate(date = lubridate::date(date_time),
         month = lubridate::month(date),
         year = lubridate::year(date))%>%
    filter(month == "8")%>%
  group_by(year)%>%
  summarize(yearly_temp_max = mean(temp_C, na.rm = T),
            yearly_temp_sd = sd(temp_C, na.rm = T))%>%
    mutate(site_code = lakes[s])

  lake_temp[[s]] <- t_max

}

lake_temps = do.call(rbind, lake_temp) %>%
  filter(year >= "2009")%>%
  mutate(year = as.numeric(year))

mean_lake_temps <- lake_temps %>%
  group_by(year)%>%
  summarize(mean = mean(yearly_temp_max, na.rm = T),
            sd = sd(yearly_temp_max, na.rm = T))

lake_swe = env_dat_yr %>%
  group_by(event_year, site_code, park_code)%>%
  summarize(SWE_may_mean = mean(SWE_May_snotel, na.rm = T),
            SWE_may_sd = sd(SWE_May_snotel, na.rm = T))%>%
  filter(event_year >= "2009")%>%
  filter(event_year< "2019")%>%
  rename(year = event_year)%>%
  mutate(year = as.numeric(year)) %>% left_join(., mean_lake_temps, by = c("year"))%>%
  mutate(SWE = scale(SWE_may_mean),
         temp = scale(mean))%>%
  mutate(year = as.character(year))


lake_swe_BMI = env_dat_yr %>%
  filter(park_code %in% c("MORA","NOCA"))%>%
  group_by(event_year)%>%
  summarize(SWE_may_mean = mean(SWE_May_snotel, na.rm = T),
            SWE_may_sd = sd(SWE_May_snotel, na.rm = T))%>%
  filter(event_year >= "2009")%>%
  filter(event_year< "2019")%>%
  rename(year = event_year)%>%
  mutate(year = as.character(year))%>%
  mutate(SWE_extreme = ifelse(SWE_may_mean <= 118,"low","norm"))%>%
  mutate(SWE_extreme = ifelse(SWE_may_mean >= 207,"high",SWE_extreme))


coeff <- 10

ggplot(lake_temps, aes(x=year)) +
  geom_col(data = lake_swe, aes(x = as.numeric(year), y=SWE_may_mean/coeff), fill = "grey", color = "black", inherit.aes = F) + # Divide by 10 to get the same range than the temperature
  geom_errorbar(data = lake_swe, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
                                ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_point(aes(y=yearly_temp_max, group = site_code), color = "black", size = 3, alpha = 0.6) +
  geom_ribbon(data = mean_lake_temps, aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.2, fill = "blue") +
  geom_line(data = mean_lake_temps, aes(x = year, y = mean), color = "blue", size = 1.5, alpha = 1)+
  scale_y_continuous(name = "Mean Ice-free Temp. (°C)",
    sec.axis = sec_axis(~.*coeff, name="May SWE Snotel (cm)")
  )+
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./figures/temp_swe_FIG1.jpg", width = 10, height = 7, units = "in", dpi = 1000)



zoops = env_zoop_data %>%
  filter(event_year >= "2009") %>%
  filter(event_year <= "2018")

lake_swe_zoops <- lake_swe %>%
  ungroup(.)%>%
  mutate(anom = SWE_may_mean - 150)%>%
  group_by(year)%>%
  summarize_all(funs(mean), na.rm = T)%>%
  mutate(SWE_extreme = ifelse(SWE_may_mean <= 119.1,"low","norm"))%>%
  mutate(SWE_extreme = ifelse(SWE_may_mean >= 194.3,"high",SWE_extreme))

mean_zoops <- zoops %>%
  group_by(event_year,park_code)%>%
  summarize(mean_CLAD = mean(CLAD, na.rm = T),
            sd_CLAD = se(CLAD),
            mean_COPE = mean(COPE, na.rm = T),
            sd_COPE = se(COPE),
            mean_MICRO = mean(MICRO, na.rm = T),
            sd_MICRO = se(MICRO),
            mean_RAP = mean(RAP, na.rm = T),
            sd_RAP = se(RAP)) %>%
  rename(year = event_year)%>%
  left_join(., lake_swe_zoops[1:10], by = "year")%>%
  ungroup(.)

library(outliers)

dixon.test(zoops$CLAD)

max(mean_zoops$mean_CLAD)

coeff = 70
CLAD <- ggplot() +
  geom_col(data = lake_swe_zoops, aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme), inherit.aes = F) + # Divide by 10 to get the same range than the temperature
  geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
                                     ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(data = mean_zoops, aes(x = as.numeric(year),y = mean_CLAD, ymin = mean_CLAD-sd_CLAD, ymax = mean_CLAD+sd_CLAD), width=.2) +
  geom_line(data = mean_zoops, aes(x = as.numeric(year), y = mean_CLAD, color = park_code.x), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "scaled relative abundance",
                     sec.axis = sec_axis(~.*coeff, name=""))+
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_color_manual(values = c("mediumorchid4","turquoise","olivedrab3"),
                     name = "Park") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Cladocerans")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "top",
        legend.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

COPE <- ggplot() +
  geom_col(data = lake_swe_zoops, aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme), inherit.aes = F) + # Divide by 10 to get the same range than the temperature
  geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
                                           ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(data = mean_zoops, aes(x = as.numeric(year),y = mean_COPE, ymin = mean_COPE-sd_COPE, ymax = mean_COPE+sd_COPE), width=.2) +
  geom_line(data = mean_zoops, aes(x = as.numeric(year), y = mean_COPE, color = park_code.x), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "",
                     sec.axis = sec_axis(~.*coeff, name="May snowpack (cm)"))+
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_color_manual(values = c("mediumorchid4","turquoise","olivedrab3"),
                     name = "Park") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Copepods")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

RAP <- ggplot() +
  geom_col(data = lake_swe_zoops, aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme), inherit.aes = F) + # Divide by 10 to get the same range than the temperature
  geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
                                           ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(data = mean_zoops, aes(x = as.numeric(year),y = mean_RAP, ymin = mean_RAP-sd_RAP, ymax = mean_RAP+sd_RAP), width=.2) +
  geom_line(data = mean_zoops, aes(x = as.numeric(year), y = mean_RAP, color = park_code.x), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "",
                     sec.axis = sec_axis(~.*coeff, name="May snowpack (cm)"))+
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_color_manual(values = c("mediumorchid4","turquoise","olivedrab3"),
                     name = "Park") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Raptorial Rotifers")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

MICRO <- ggplot() +
  geom_col(data = lake_swe_zoops, aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme), inherit.aes = F) + # Divide by 10 to get the same range than the temperature
  geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
                                           ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(data = mean_zoops, aes(x = as.numeric(year),y = mean_MICRO, ymin = mean_MICRO-sd_MICRO, ymax = mean_MICRO+sd_MICRO), width=.2) +
  geom_line(data = mean_zoops, aes(x = as.numeric(year), y = mean_MICRO, color = park_code.x), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "scaled relative abundance",
                     sec.axis = sec_axis(~.*coeff, name=""))+
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_color_manual(values = c("mediumorchid4","turquoise","olivedrab3"),
                     name = "Park") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Microphagous Rotifers")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

fig <- (CLAD+COPE)/(MICRO+RAP)
fig
ggsave("./figures/ZOOP_swe_FIG.jpg", width = 12, height = 10, units = "in", dpi = 1000)








mean_zoops_2 <- zoops %>%
  group_by(event_year)%>%
  summarize(mean_CLAD = mean(CLAD, na.rm = T),
            sd_CLAD = sd(CLAD, na.rm = T),
            mean_COPE = mean(COPE, na.rm = T),
            sd_COPE = sd(COPE, na.rm = T),
            mean_MICRO = mean(MICRO, na.rm = T),
            sd_MICRO = sd(MICRO, na.rm = T),
            mean_RAP = mean(RAP, na.rm = T),
            sd_RAP = sd(RAP, na.rm = T)) %>%
  rename(year = event_year)%>%
  left_join(., lake_swe, by = "year") %>%
  melt(., id.var = c("year","SWE_may_mean"))%>%
  na.omit(.)

mean_zoops_2 %>% filter(grepl('mean_', variable)) %>%
ggplot(.)+
  geom_smooth(aes(x = SWE_may_mean, y = value, group = variable, color = variable, fill = variable),method = lm)+
  geom_point(aes(x = SWE_may_mean, y = value, group = variable, fill = variable), color = "black", pch = 21, size = 4)


all_BMI <- macro_join_rda %>%
  ungroup(.)%>%
  mutate(FCR = ifelse(is.na(FCR),0,FCR),
         GCR = ifelse(is.na(GCR),0,GCR),
         PRED = ifelse(is.na(PRED),0,PRED),
         SCR = ifelse(is.na(SCR),0,SCR),
         SHR = ifelse(is.na(SHR),0,SHR))%>%
  group_by(event_year, park_code, site_code)%>%
  summarize_at(vars(FCR, GCR, PRED, SCR, SHR),funs(sum))%>%
  melt(., id.vars = c("event_year", "site_code", "park_code"))%>%
  filter(variable %in% c("FCR","GCR","PRED","SCR","SHR"))%>%
  rename(year = event_year)%>%
  na.omit(.) %>% ungroup(.)


mean_BMI <- all_BMI %>%  ungroup(.) %>%
  left_join(., lake_swe_BMI, by = "year")%>%
  na.omit(.) %>%
  select(-site_code)%>% group_by(year, park_code, variable, SWE_extreme)%>%
  mutate_at(vars(value),funs(log10(.+1)))%>%
  mutate(sd_ffg = se(value))%>%
  summarize_all(funs(mean))





coeff = 200

PRED <- mean_BMI %>% filter(year>"2008")%>%filter(variable == "PRED") %>% ggplot(.) +
  geom_col(aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme)) + # Divide by 10 to get the same range than the temperature
  # geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
  #                                          ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(aes(x = as.numeric(year), y = value , ymin = value -sd_ffg, ymax = value +sd_ffg),color = "black", width = 0.2) +
  geom_line(aes(x = as.numeric(year), y = value , color = park_code), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "scaled relative abundance",
                     sec.axis = sec_axis(~.*coeff, name=""))+
  scale_color_manual(values = c("mediumorchid4","turquoise"),
                     name = "Park") +
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Predator BMIs")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "top",
        legend.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))
PRED

SHR <- mean_BMI %>% filter(year>"2008")%>%filter(variable == "SHR") %>% ggplot(.) +
  geom_col(aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme)) + # Divide by 10 to get the same range than the temperature
  # geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
  #                                          ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(aes(x = as.numeric(year), y = value , ymin = value -sd_ffg, ymax = value +sd_ffg),color = "black", width = 0.2) +
  geom_line(aes(x = as.numeric(year), y = value , color = park_code), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "",
                     sec.axis = sec_axis(~.*coeff, name=""))+
  scale_color_manual(values = c("mediumorchid4","turquoise"),
                     name = "Park") +
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Shredder BMIs")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))



GCR <- mean_BMI %>% filter(year>"2008")%>%filter(variable == "GCR") %>% ggplot(.) +
  geom_col(aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme)) + # Divide by 10 to get the same range than the temperature
  # geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
  #                                          ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(aes(x = as.numeric(year), y = value , ymin = value -sd_ffg, ymax = value +sd_ffg),color = "black", width = 0.2) +
  geom_line(aes(x = as.numeric(year), y = value , color = park_code), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "",
                     sec.axis = sec_axis(~.*coeff, name="May snowpack (SWE)"))+
  scale_color_manual(values = c("mediumorchid4","turquoise"),
                     name = "Park") +
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Gatherer BMIs")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

FCR <- mean_BMI %>% filter(year>"2008")%>%filter(variable == "FCR") %>% ggplot(.) +
  geom_col(aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme)) + # Divide by 10 to get the same range than the temperature
  # geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
  #                                          ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(aes(x = as.numeric(year), y = value , ymin = value -sd_ffg, ymax = value +sd_ffg),color = "black", width = 0.2) +
  geom_line(aes(x = as.numeric(year), y = value , color = park_code), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "",
                     sec.axis = sec_axis(~.*coeff, name="May snowpack (SWE)"))+
  scale_color_manual(values = c("mediumorchid4","turquoise"),
                     name = "Park") +
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Filterer BMIs")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))


SCR <- mean_BMI %>% filter(year>"2008")%>%filter(variable == "SCR") %>% ggplot(.) +
  geom_col(aes(x = as.numeric(year), y=SWE_may_mean/coeff, fill = SWE_extreme)) + # Divide by 10 to get the same range than the temperature
  # geom_errorbar(data = lake_swe_zoops, aes(x=as.numeric(year), ymin = SWE_may_mean/coeff,
  #                                          ymax = SWE_may_mean/coeff+SWE_may_sd/coeff), width=.2)+
  geom_errorbar(aes(x = as.numeric(year), y = value , ymin = value -sd_ffg, ymax = value +sd_ffg),color = "black", width = 0.2) +
  geom_line(aes(x = as.numeric(year), y = value , color = park_code), size = 1.5, alpha = 1)+
  scale_y_continuous(name = "scaled relative abundance",
                     sec.axis = sec_axis(~.*coeff, name=""))+
  scale_color_manual(values = c("mediumorchid4","turquoise"),
                    name = "Park") +
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Snowpack Extremes") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_classic()+
  xlab("")+
  labs(title = "Scraper-Grazer BMIs")+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 15),legend.position = "none",
        legend.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))


fig2 <- (PRED+SHR+GCR)/(SCR+FCR+plot_spacer())
fig2
ggsave("./figures/BMI_swe_FIG.jpg", width = 16, height = 10, units = "in", dpi = 1000)

#
#
# for(s in 1:length(lakes)){
#
#   Temp_mean <- list.files(path, pattern = paste0("_",lakes[s],"_")) %>%
#     map(~read_csv(paste0(path,.), col_types = cols(), col_names = FALSE, id = "file_name", skip = 4))%>%
#     data.table::rbindlist(fill = T)%>%
#     rename(date_time = X1,
#            temp_C = X2)%>%
#     select(-X3,-X4,-X5)%>%
#     mutate(file_name = case_when(
#       file_name == paste0("./data/temp_time_series/AirTemp_",lakes[s],"_POR.csv") ~ "AirTemp",
#       file_name == paste0("./data/temp_time_series/BottomTemp_",lakes[s],"_POR.csv") ~ "BottomTemp",
#       file_name == paste0("./data/temp_time_series/MidTemp_",lakes[s],"_POR.csv") ~ "MidTemp",
#       file_name == paste0("./data/temp_time_series/SurfaceTemp_",lakes[s],"_POR.csv") ~ "SurfTemp",
#       TRUE ~ NA_character_))%>%
#     mutate(date_time = lubridate::mdy_hms(date_time))%>%
#     filter(file_name!="AirTemp")%>%
#     mutate(date = lubridate::date(date_time))%>%
#     group_by(date)%>%
#     summarize(daily_temp_mean = max(temp_C, na.rm = T),
#               daily_temp_sd = sd(temp_C, na.rm = T),
#               daily_temp_CV = abs(daily_temp_sd/daily_temp_mean))%>%
#     reshape2::melt(., id = "date")%>%
#     filter(variable == "daily_temp_mean")%>%
#     mutate(month = lubridate::month(date),
#            year = lubridate::year(date))
#
#   write_csv(Temp_mean, paste0("./data/Temp_mean_",lakes[s],".csv"))
#
#     temp_mean_plot <- ggplot(Temp_mean, aes(date, value))+
#     geom_line()+
#     theme_bw()
#
#     temp_mean_plot
#
#   ggsave(path = ".", filename = paste0("./figures/Temp_mean_",lakes[s],".jpg"),
#          width = 18, height = 12, device = "jpg", dpi = 400)
#
#
# }
#
#
# for(s in 1:length(lakes)){
#
#   AirTemp_mean <- list.files(path, pattern = paste0("_",lakes[s],"_")) %>%
#     map(~read_csv(paste0(path,.), col_types = cols(), col_names = FALSE, id = "file_name", skip = 4))%>%
#     data.table::rbindlist(fill = T)%>%
#     rename(date_time = X1,
#            temp_C = X2)%>%
#     select(-X3,-X4,-X5)%>%
#     mutate(file_name = case_when(
#       file_name == paste0("./data/temp_time_series/AirTemp_",lakes[s],"_POR.csv") ~ "AirTemp",
#       file_name == paste0("./data/temp_time_series/BottomTemp_",lakes[s],"_POR.csv") ~ "BottomTemp",
#       file_name == paste0("./data/temp_time_series/MidTemp_",lakes[s],"_POR.csv") ~ "MidTemp",
#       file_name == paste0("./data/temp_time_series/SurfaceTemp_",lakes[s],"_POR.csv") ~ "SurfTemp",
#       TRUE ~ NA_character_))%>%
#     mutate(date_time = lubridate::mdy_hms(date_time))%>%
#     filter(file_name=="AirTemp")%>%
#     mutate(date = lubridate::date(date_time))%>%
#     group_by(date)%>%
#     summarize(daily_temp_mean = mean(temp_C, na.rm = T),
#               daily_temp_sd = sd(temp_C, na.rm = T),
#               daily_temp_CV = abs(daily_temp_sd/daily_temp_mean))%>%
#     reshape2::melt(., id = "date")%>%
#     filter(variable == "daily_temp_mean")%>%
#     mutate(month = lubridate::month(date),
#            year = lubridate::year(date))%>%
#     ggplot(., aes(date, value))+
#     geom_line()+
#     theme_bw()
#
#   AirTemp_mean
#
#   ggsave(path = ".", filename = paste0("./figures/AirTemp_mean_",lakes[s],".jpg"),
#          width = 18, height = 12, device = "jpg", dpi = 400)
#
# }
#
#
#
# for(s in 1:length(lakes)){
#
#   Temp_mean <- list.files(path, pattern = paste0("_",lakes[s],"_")) %>%
#     map(~read_csv(paste0(path,.), col_types = cols(), col_names = FALSE, id = "file_name", skip = 4))%>%
#     data.table::rbindlist(fill = T)%>%
#     rename(date_time = X1,
#            temp_C = X2)%>%
#     select(-X3,-X4,-X5)%>%
#     mutate(file_name = case_when(
#       file_name == paste0("./data/temp_time_series/AirTemp_",lakes[s],"_POR.csv") ~ "AirTemp",
#       file_name == paste0("./data/temp_time_series/BottomTemp_",lakes[s],"_POR.csv") ~ "BottomTemp",
#       file_name == paste0("./data/temp_time_series/MidTemp_",lakes[s],"_POR.csv") ~ "MidTemp",
#       file_name == paste0("./data/temp_time_series/SurfaceTemp_",lakes[s],"_POR.csv") ~ "SurfTemp",
#       TRUE ~ NA_character_))%>%
#     mutate(date_time = lubridate::mdy_hms(date_time))%>%
#     dplyr::filter(file_name %in% c("BottomTemp", "MidTemp", "SurfTemp"))%>%
#     mutate(date = lubridate::date(date_time))%>%
#     group_by(date)%>%
#     summarize(daily_temp_mean = mean(temp_C, na.rm = T),
#               daily_temp_sd = sd(temp_C, na.rm = T),
#               daily_temp_CV = abs(daily_temp_sd/daily_temp_mean))%>%
#     reshape2::melt(., id = "date")%>%
#     filter(variable == "daily_temp_mean")%>%
#     mutate(month = lubridate::month(date),
#            year = lubridate::year(date))%>%
#     ggplot(., aes(date, value))+
#     geom_line()+
#     theme_bw()
#
#   Temp_mean
#
#   ggsave(path = ".", filename = paste0("./figures/Temp_mean_",lakes[s],".jpg"),
#          width = 18, height = 12, device = "jpg", dpi = 400)
#
# }
#
#
#
#
# BottomTemp_Lake <- list.files(path, pattern = paste0("BottomTemp_")) %>%
#     map(~read_csv(paste0(path,.), col_types = cols(), col_names = FALSE, id = "file_name", skip = 4))%>%
#     data.table::rbindlist(fill = T)%>%
#     rename(date_time = X1,
#            temp_C = X2)%>%
#     select(-X3,-X4,-X5)%>%
#     mutate(file_name = case_when(
#       file_name == paste0("./data/temp_time_series/BottomTemp_Connie_POR.csv") ~ "Connie",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Crazy_POR.csv") ~ "Crazy",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Ferry_POR.csv") ~ "Ferry",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Gladys_POR.csv") ~ "Gladys",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Heather_POR.csv") ~ "Heather",
#       file_name == paste0("./data/temp_time_series/BottomTemp_LaCrosse_POR.csv") ~ "LaCrosse",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Milk_POR.csv") ~ "Milk",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Sunup_POR.csv") ~ "Sunup",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Allen_POR.csv") ~ "Allen",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Blue_POR.csv") ~ "Blue",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Bowan_POR.csv") ~ "Bowan",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Deadwood_POR.csv") ~ "Deadwood",
#       file_name == paste0("./data/temp_time_series/BottomTemp_EasyRidge_POR.csv") ~ "EasyRidge",
#       file_name == paste0("./data/temp_time_series/BottomTemp_LH15_POR.csv") ~ "LH15",
#       file_name == paste0("./data/temp_time_series/BottomTemp_LowerBlum_POR.csv") ~ "LowerBlum",
#       file_name == paste0("./data/temp_time_series/BottomTemp_LowerEast_POR.csv") ~ "LowerEast",
#       file_name == paste0("./data/temp_time_series/BottomTemp_LowerSilent_POR.csv") ~ "LowerSilent",
#       file_name == paste0("./data/temp_time_series/BottomTemp_LP19_POR.csv") ~ "LP19",
#       file_name == paste0("./data/temp_time_series/BottomTemp_Pallisades_POR.csv") ~ "Pallisades",
#       file_name == paste0("./data/temp_time_series/BottomTemp_UpperTriplet_POR.csv") ~ "UpperTriplet",
#       TRUE ~ NA_character_))%>%
#     mutate(date_time = lubridate::mdy_hms(date_time))%>%
#     mutate(date = lubridate::date(date_time))%>%
#     group_by(date, file_name)%>%
#     summarize(bottom_temp_mean = mean(temp_C, na.rm = T),
#               bottom_temp_se = se(temp_C))%>%
#   arrange(file_name)%>%
#   mutate(year = lubridate::year(date))%>%
#   group_by(file_name, year)%>%
#   summarize(max_temp = max(bottom_temp_mean))%>%
#   summarize
#
# ggplot(BottomTemp_Lake, aes(year, max_temp))+
#   geom_point(size = 2)+
#   geom_smooth(method = "lm")+
#   theme_bw()+
#   facet_wrap(~file_name, scales = "free_y")
#
# AirTemp_Lake <- list.files(path, pattern = paste0("AirTemp_")) %>%
#   map(~read_csv(paste0(path,.), col_types = cols(), col_names = FALSE, id = "file_name", skip = 4))%>%
#   data.table::rbindlist(fill = T)%>%
#   rename(date_time = X1,
#          temp_C = X2)%>%
#   select(-X3,-X4,-X5)%>%
#   mutate(file_name = case_when(
#     file_name == paste0("./data/temp_time_series/AirTemp_Connie_POR.csv") ~ "Connie",
#     file_name == paste0("./data/temp_time_series/AirTemp_Crazy_POR.csv") ~ "Crazy",
#     file_name == paste0("./data/temp_time_series/AirTemp_Ferry_POR.csv") ~ "Ferry",
#     file_name == paste0("./data/temp_time_series/AirTemp_Gladys_POR.csv") ~ "Gladys",
#     file_name == paste0("./data/temp_time_series/AirTemp_Heather_POR.csv") ~ "Heather",
#     file_name == paste0("./data/temp_time_series/AirTemp_LaCrosse_POR.csv") ~ "LaCrosse",
#     file_name == paste0("./data/temp_time_series/AirTemp_Milk_POR.csv") ~ "Milk",
#     file_name == paste0("./data/temp_time_series/AirTemp_Sunup_POR.csv") ~ "Sunup",
#     file_name == paste0("./data/temp_time_series/AirTemp_Allen_POR.csv") ~ "Allen",
#     file_name == paste0("./data/temp_time_series/AirTemp_Blue_POR.csv") ~ "Blue",
#     file_name == paste0("./data/temp_time_series/AirTemp_Bowan_POR.csv") ~ "Bowan",
#     file_name == paste0("./data/temp_time_series/AirTemp_Deadwood_POR.csv") ~ "Deadwood",
#     file_name == paste0("./data/temp_time_series/AirTemp_EasyRidge_POR.csv") ~ "EasyRidge",
#     file_name == paste0("./data/temp_time_series/AirTemp_LH15_POR.csv") ~ "LH15",
#     file_name == paste0("./data/temp_time_series/AirTemp_LowerBlum_POR.csv") ~ "LowerBlum",
#     file_name == paste0("./data/temp_time_series/AirTemp_LowerEast_POR.csv") ~ "LowerEast",
#     file_name == paste0("./data/temp_time_series/AirTemp_LowerSilent_POR.csv") ~ "LowerSilent",
#     file_name == paste0("./data/temp_time_series/AirTemp_LP19_POR.csv") ~ "LP19",
#     file_name == paste0("./data/temp_time_series/AirTemp_Pallisades_POR.csv") ~ "Pallisades",
#     file_name == paste0("./data/temp_time_series/AirTemp_UpperTriplet_POR.csv") ~ "UpperTriplet",
#     TRUE ~ NA_character_))%>%
#   mutate(date_time = lubridate::mdy_hms(date_time))%>%
#   mutate(date = lubridate::date(date_time))%>%
#   group_by(date, file_name)%>%
#   summarize(air_temp_mean = mean(temp_C, na.rm = T),
#             air_temp_se = se(temp_C))%>%
#   arrange(file_name)%>%
#   mutate(year = lubridate::year(date))%>%
#   group_by(file_name, year)%>%
#   summarize(max_temp = max(air_temp_mean))
#
# ggplot(AirTemp_Lake, aes(year, max_temp))+
#   geom_point(size = 2)+
#   geom_smooth(method = "lm")+
#   theme_bw()+
#   facet_wrap(~file_name, scales = "free_y")
#
# data_join <- left_join(BottomTemp_Lake, AirTemp_Lake, by = c("year", "file_name"))
#
