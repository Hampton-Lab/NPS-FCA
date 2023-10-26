library(tidyverse)

NPS_profiles <- read_csv("./data/NPS_NCCN_Mtn_Lakes_Exports/qs_b364_Water_Column_Profile_Data_20200723_160058.csv")

do <- NPS_profiles %>%
  filter(Parameter == "DO")%>%
  mutate(Parameter_value = ifelse(Parameter_value<1.0,NA,Parameter_value))%>%
  select(Park_code, Site_name, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Site_name, Event_year, Depth_bin_m)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Site_name)%>%
  ggplot(., aes(Parameter_value, -Depth_bin_m, color = as.factor(Event_year)))+
  geom_line()+
  facet_wrap(~Site_name, scales = "free")+
  theme(legend.position = "bottom")

do_hypo <- NPS_profiles %>%
  filter(Parameter == "DO")%>%
  mutate(Parameter_value = ifelse(Parameter_value<1.0,NA,Parameter_value))%>%
  mutate(Parameter_value = ifelse(Parameter_value>20.0,NA,Parameter_value))%>%
  select(Park_code, Site_name, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Depth_bin_m, Site_name)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Event_year, Site_name)%>%
  filter(Depth_bin_m == max(Depth_bin_m))%>%
  ggplot(., aes(Event_year, Parameter_value, color = as.character(Site_name)))+
  geom_point(size = 2)+
  theme_classic()+
  theme(legend.position = "bottom")


temp <- NPS_profiles %>%
  filter(Parameter == "Temperature")%>%
  mutate(Parameter_value = ifelse(Parameter_value<1.0,NA,Parameter_value))%>%
  select(Park_code, Site_name, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Depth_bin_m, Site_name)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Site_name)%>%
  ggplot(., aes(Parameter_value, -Depth_bin_m, color = as.character(Event_year)))+
  geom_point(size = 0.7)+
  geom_line(lwd = 0.7)+
  facet_wrap(~Site_name, scales = "free")+
  theme_classic()+
  theme(legend.position = "bottom")


min.freq = 0.5

temp_hypo <- NPS_profiles %>%
  filter(Parameter == "Temperature")%>%
  mutate(Parameter_value = ifelse(Parameter_value<1.0,NA,Parameter_value))%>%
  select(Park_code, Site_name, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Depth_bin_m, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Event_year, Park_code)%>%
  filter(Depth_bin_m > max(Depth_bin_m)-2)%>%
  group_by(Event_year, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  ggplot(., aes(Event_year, Parameter_value, color = as.character(Park_code)))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = T, alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")

temp_surf <- NPS_profiles %>%
  filter(Parameter == "Temperature")%>%
  mutate(Parameter_value = ifelse(Parameter_value<1.0,NA,Parameter_value))%>%
  select(Park_code, Site_name, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Depth_bin_m, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Event_year, Park_code)%>%
  filter(Depth_bin_m < max(Depth_bin_m)-1.5)%>%
  group_by(Event_year, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  ggplot(., aes(Event_year, Parameter_value, color = as.character(Park_code)))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = T, alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")

temp_whole <- NPS_profiles %>%
  filter(Parameter == "Temperature")%>%
  mutate(Parameter_value = ifelse(Parameter_value<1.0,NA,Parameter_value))%>%
  select(Park_code, Site_name, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Depth_bin_m, Site_name)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Event_year)%>%
  summarize(sd = sd(Parameter_value, na.rm = T),
            Parameter_value = mean(Parameter_value, na.rm = T))%>%
  ggplot(.)+
  geom_errorbar(mapping = aes(Event_year, Parameter_value, ymin=Parameter_value-sd, ymax=Parameter_value+sd), width=0.2, size=1, color="blue")+
  geom_point(aes(Event_year, Parameter_value), size = 4)+
  geom_smooth(aes(Event_year, Parameter_value), method = "lm", se = T, alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")


temp_whole <- NPS_profiles %>%
  filter(Parameter == "Temperature")%>%
  mutate(Parameter_value = ifelse(Parameter_value<1.0,NA,Parameter_value))%>%
  select(Park_code, Site_code, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Site_code, Park_code)%>%
  summarize(sd = sd(Parameter_value, na.rm = T),
            Parameter_value = mean(Parameter_value, na.rm = T))%>%
  group_by(Event_year)%>%
  summarize(sd = sd(Parameter_value, na.rm = T),
            Parameter_value = mean(Parameter_value, na.rm = T))%>%
  ggplot(.)+
  geom_errorbar(mapping = aes(Event_year, Parameter_value, ymin=Parameter_value-sd, ymax=Parameter_value+sd), width=0.2, size=1, color="blue")+
  geom_point(aes(Event_year, Parameter_value), size = 4)+
  geom_smooth(aes(Event_year, Parameter_value), method = "lm", se = T, alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")

#Specific conductance

spc <- NPS_profiles %>%
  filter(Parameter == "SpCond")%>%
  select(Park_code, Site_name, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Site_name, Event_year, Depth_bin_m)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Site_name)%>%
  ggplot(., aes(Parameter_value, -Depth_bin_m, color = as.factor(Event_year)))+
  geom_line()+
  facet_wrap(~Site_name, scales = "free")+
  theme(legend.position = "bottom")

spc_whole <- NPS_profiles %>%
  filter(Parameter == "SpCond")%>%
  mutate(Parameter_value = ifelse(Parameter_value>500,NA,Parameter_value))%>%
  select(Park_code, Site_code, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Depth_bin_m, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Event_year, Park_code)%>%
  group_by(Event_year, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  ggplot(., aes(Event_year, Parameter_value, color = as.character(Park_code)))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = T, alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")


chloro_whole <- NPS_profiles %>%
  filter(Parameter == "Chlorophyll")%>%
  mutate(Parameter_value = ifelse(Parameter_value>500,NA,Parameter_value))%>%
  select(Park_code, Site_code, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Depth_bin_m, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Event_year, Park_code)%>%
  group_by(Event_year, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  ggplot(., aes(Event_year, Parameter_value, color = as.character(Park_code)))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = T, alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")

pH_whole <- NPS_profiles %>%
  filter(Parameter == "pH")%>%
  mutate(Parameter_value = ifelse(Parameter_value>500,NA,Parameter_value))%>%
  select(Park_code, Site_code, Timestamp, Event_year, Depth_bin_m, Parameter_value)%>%
  mutate(Depth_bin_m = ifelse(Depth_bin_m<0,-Depth_bin_m,Depth_bin_m))%>%
  group_by(Event_year, Depth_bin_m, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  group_by(Event_year, Park_code)%>%
  group_by(Event_year, Park_code)%>%
  summarize(Parameter_value = mean(Parameter_value))%>%
  ggplot(., aes(Event_year, Parameter_value, color = as.character(Park_code)))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = T, alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")

