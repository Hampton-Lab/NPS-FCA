# This script follows up on bigjoin.R by taking its output and using it
# for modeling and plotting purposes. It is intended for the production of
# a small number of specific figures.

# 1. Load packages --------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggrepel)
library(ggpubr)
library(gridExtra)

# 2. Load bigjoin ---------------------------------------------------------

bigjoin <- readRDS(file = file.path("..",
                                    "data",
                                    "analysis_outputs",
                                    "bigjoin.rds"))

# Add a column of shortened names for each lake for labels on plots
short_codes <- tribble(
  ~site_code, ~short_code,
  "LH14",     "PA",
  "LH15",     "15",
  "LN03",     "AL",
  "LP19",     "19",
  "LW32",     "DW",
  "LZ35",     "BL",
  "LS-07-01",     "LB",
  "MA-03-01",     "SI",
  "MC-03-01",     "ER",
  "MC-14-02",     "EA",
  "MR-12-01",     "BO",
  "SM-02-02",     "TR",
  "106",     "GL",
  "138",     "FE",
  "263",     "HE",
  "426",     "CR",
  "498",     "MI",
  "520",     "LC",
  "623",     "SU",
  "627",     "CO",
  "Hoh",     "HO"
)


bigjoin <- full_join(x = bigjoin, y = short_codes, by = c("site_code"))

lake_key<-unique(bigjoin %>% select(park_code,short_code,park_site)) %>% arrange(park_code,short_code)
lake_key

axis_label_tbl <- tribble(
  ~variable, ~axis_label,
  "secchi_value_m",     "Secchi depth (m)",
  "ProfTemp_top2m", paste0("Water temperature (", intToUtf8(176), "C), top 2m"),
  "SWE_May", "May SWE (cm)",
  "DO_top2m", "D.O. (mg/L), top 2m",
  "pH_top2m", "pH, top 2m",
  "SpCond_top2m", paste0("Sp. conductivity (", intToUtf8(956), "S/cm), top 2m"),
  "Chlorophyll", paste0("Chlorophyll (", intToUtf8(956), "g/L)"),
  "Na", "Na (mg/L)",
  "Cl", "Cl (mg/L)",
  "Mg", "Mg (mg/L)",
  "Ca", "Ca (mg/L)",
  "SO4", "SO4 (mg/L)",
  "Total N", "Total dissolved N (mg/L)",
  "ice_out_doy","Ice out day of year",
  "flush_index_SWE_May","Catchment snow vol : lake vol"
)

bigjoin <- full_join(x = bigjoin, y = axis_label_tbl, by = c("variable"))

dataplot<-bigjoin %>% filter(year %in% c(2011,2015), variable %in% c("ProfTemp_top2m"))

dataplot$xaxis_label<-"Log10 Catchment snow vol : lake vol"

plot_wtempSWE<-ggplot(dataplot, aes(x=SWE_May, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$year==2015)) + 
#  scale_x_log10() +
  xlab("May snow water equivalent-SWE (cm)")+
  ylab("Value")+
  theme_bw()+
#  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+
  ylab(paste0("Water temperature (", intToUtf8(176), "C), top 2m"))

plot_wtemp_flush<-ggplot(dataplot, aes(x=flush_index_SWE_May, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$year==2015)) + 
  scale_x_log10() +
  xlab(dataplot$xaxis_label)+
  ylab("Value")+
  theme_bw()+
  #  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+
  ylab(paste0("Water temperature (", intToUtf8(176), "C), top 2m"))


png(file="../figures/wtemp_snow.png",width=6,height=2.8,units="in",res=600)
grid.arrange(plot_wtempSWE,plot_wtemp_flush,ncol=2)
dev.off()

dataplot<-unique(subset(bigjoin,bigjoin$variable=="SWE_May") %>% select(park_code,Snowsite,year,value))
SWE_timeseries <- 
  ggplot(dataplot,aes(x = year, y = value, color = park_code,group=Snowsite,label=Snowsite)) +
  geom_point()+#aes(x = year, y = value, color = park_code,group=Snowsite)) +
  geom_line()+#aes(x = year, y = value, color = park_code,group=Snowsite)) +
  geom_text_repel(size=4,segment.color="black",segment.size = 0.5,box.padding=0.5,#alpha=1,
                  data=subset(dataplot,dataplot$year==2018)) + 
#  facet_grid(rows = vars(Snowsite)) +
  ylab("May snowpack - SWE (cm)")+
  xlab("Year") +
#  xlim(2011,2019)+
  theme_bw() +
  scale_color_discrete(name = "Park")+
  theme(legend.position="none")+
  scale_x_continuous(breaks=c(2011:2019),limits=c(2011,2019))

png(file="../figures/swe_timeseries.png",width=6,height=2.8,units="in",res=300)
SWE_timeseries
dev.off()



dataplot<-bigjoin %>% filter(year %in% c(2011,2015), variable %in% c("DO_top2m","Chlorophyll", #"Chlorophyll_log10", 
                                                                     "secchi_value_m" , "pH_top2m"))

dataplot$xaxis_label<-"Log10 Catchment snow vol : lake vol"
dataplot$value[which(dataplot$short_code=="TR" & dataplot$year==2015 & 
                       dataplot$variable=="Chlorophyll")]<-2.5 
dataplot$short_code[which(dataplot$short_code=="TR" &  
                       dataplot$variable=="Chlorophyll")]<-"TR*"

plot_limnoflush<-ggplot(dataplot, aes(x=flush_index_SWE_May, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$year==2015)) + 
  scale_x_log10() +
  xlab(dataplot$xaxis_label)+
  ylab("Value")+
  theme_bw()+
  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))

png(file="../figures/limnoflush.png",width=4.5,height=4,units="in",res=600)
plot_limnoflush
dev.off()

dataplot<-bigjoin %>% filter(year %in% c(2011,2015), variable %in% c("Na", "Mg", "SO4","Total N"))
dataplot$xaxis_label<-"Log10 Catchment snow vol : lake vol"
dataplot$value[which(dataplot$short_code=="EA" & dataplot$year==2011 & 
                       dataplot$variable=="SO4")]<-8.5 
dataplot$short_code[which(dataplot$short_code=="EA" &  
                            dataplot$variable=="SO4")]<-"EA*"

plot_chemsflush<-ggplot(dataplot, aes(x=flush_index_SWE_May, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$year==2015)) + 
  scale_x_log10() +
  xlab(dataplot$xaxis_label)+
  ylab("Value")+
  theme_bw()+
  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))

png(file="../figures/chemsflush.png",width=4.5,height=4,units="in",res=600)
plot_chemsflush
dev.off()


dataplot<-bigjoin %>% filter(year %in% c(2011,2015), variable %in% c("ice_out_doy"))
dataplot$xaxis_label<-"May SWE (cm)"
dataplot<-unique(dataplot %>% select(value,year,SWE_May,short_code,park_code,axis_label,xaxis_label))

plot_iceout_SWEmay<-ggplot(dataplot, aes(x=SWE_May, y=value, group=short_code, color=(park_code),label=short_code)) +
  geom_line(alpha=0.5)+
#  geom_point(size=4,color="white")+
#  geom_text(size=3,alpha=1)+
  #  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2)+#+,#segment.size = 2,box.padding=0.1,alpha=1,
#                  data=subset(dataplot,dataplot$year==2015)) + 
#  scale_x_log10() +
  xlab(dataplot$xaxis_label)+
  ylab(dataplot$axis_label)+
  theme_bw()+
#  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))

png(file="../figures/iceout_SWEmay.png",width=4.5,height=4,units="in",res=600)
plot_iceout_SWEmay
dev.off()


#####################################




#log10Chl<-subset(bigjoin,bigjoin$variable=="Chlorophyll")
#log10Chl$value<-log10(log10Chl$value+0.03)
#log10Chl$variable<-"Chlorophyll_log10"
#log10Chl$axis_label<-paste0("Log10 Chlorophyll (", intToUtf8(956), "g/L)")
#bigjoin2<-rbind(bigjoin,log10Chl)


dataplot_labloc<- dataplot %>% group_by(variable,short_code,park_code) %>% 
  dplyr::summarize(midx=max(log10(flush_index_SWE_May+0.01),na.rm=TRUE),
                   midy=max(value,na.rm=TRUE)) %>% as.data.frame()
dataplot<-merge(dataplot,dataplot_labloc,by=c("variable","short_code","park_code"))
plot_limnoflush<-ggplot(dataplot, aes(x=log10(flush_index_SWE_May+0.01), y=value, group="black", color=as.factor(park_code))) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text(data = subset(dataplot,dataplot$year==2015), size=2, nudge_x = 0,
            aes(x = (midx), y = midy, label = short_code, color = park_code)) +
  #  geom_text_repel(
  #    aes(label = short_code),
  #    data = . %>% filter(year == 2011),
  #    segment.color = "gray",
  #    size = 2)+
  #  scale_x_log10()+
  xlab(dataplot$xaxis_label)+
  theme_bw()+
  facet_wrap(~axis_label,ncol=3,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text())


  geom_text(aes(label=short_code,data=dataplot_centers))
  
  





plot.icedur <-  ggplot(dataplot, aes(x=synthyear, y=icedur, group="black", fill="black")) +
  geom_point(size=0.75) + ylab("Ice Duration (days)") + xlab("Winter Year")+ylim(70,180)+xlim(1980,2017)+
  theme_bw()+
  geom_smooth(linetype="dashed",method="lm",se=FALSE)+
  facet_wrap(~lakename,ncol=3) +
  theme(legend.position="none")+
  theme(strip.text.x=element_text())#+
plot.icedur












# 3. General plots --------------------------------------------------------

# SWE ~ year plot
SWE_timeseries <- bigjoin %>%
  filter(variable %in% c("SWE_May", "SWE_April")) %>%
  ggplot() +
  geom_point(aes(x = year, y = value, color = park_code)) +
  facet_grid(rows = vars(variable)) +
  ylab("") +
  xlab("Year") +
  theme_bw() +
  scale_color_discrete(name = "Park")

ggsave(filename = "../figures/SWE_time_series.png", plot = SWE_timeseries,
       device = "png", width = 7, height = 4)

# Ice in/out ~ year plot
ice_timeseries <- bigjoin %>%
  filter(variable %in% c("ice_in_doy", "ice_out_doy")) %>%
  ggplot() +
  geom_point(aes(x = year, y = value, color = park_code)) +
  facet_grid(rows = vars(variable)) +
  ylab("") +
  xlab("Year") +
  theme_bw() +
  scale_color_discrete(name = "Park")

ggsave(filename = "../figures/ice_time_series.png", plot = ice_timeseries,
       device = "png", width = 7, height = 4)


# 4. Create plot grids ----------------------------------------------------


# 4a. Function definition -------------------------------------------------

# A function to allow plotting of any two vars from bigjoin$variables
# against each other for the years 2011 & 2015 only
two_yr_line_plot_fun <- function(y_variable_name, x_variable_name) {
  
  # If we have variables that are known offenders w.r.t. outliers,
  # add 0.1 to measurements and take log10 to aid in viz

    # Subset bigjoin to vars & time of interest
    filtered_data <- dplyr::filter(.data = bigjoin,
                                   variable %in% c(y_variable_name,
                                                   x_variable_name),
                                   year %in% c(2011, 2015)) %>%
      # We don't need most of the remaining columns
      select(park_code, site_code, short_code, year, variable, value,
             axis_label)
  
  
  # Separate into different dfs using variable col, so each variable can be its
  # own column for plotting purposes. Makes a list of dfs.
  separated_dfs <-  map(.x = c(unique(filtered_data$variable)),
                        .f = ~ filter(.data = filtered_data, variable == .x))
  
  y_label <- unique(separated_dfs[[1]]$axis_label)
  x_label <- unique(separated_dfs[[2]]$axis_label)
  
  # Spread each df in above list so that variable is its own column, not stored
  # under generic "variable" key column. Note that spread() has been replaced
  # with pivot_wider(), which allows to average rows that aren't unique in ID.
  separated_dfs_wide <- map(.x = separated_dfs,
                            .f = ~ pivot_wider(data = .x,
                                               names_from = variable,
                                               values_from =  value,
                                               values_fn = list(value = mean)))
  
  # Join the two dfs into one with both vars as their own columns
  joined_dfs <- full_join(x = separated_dfs_wide[[1]],
                          y = separated_dfs_wide[[2]],
                          by = c("park_code", "site_code",
                                 "short_code", "year")) %>%
    clean_names()
  
  # Randomly choose sites to label at 2011 or 2015 points on fig
  label_flags <- joined_dfs %>%
    select(short_code) %>%
    unique() %>%
    mutate(flag = sample(x = c(0, 1), size = 21, replace = TRUE))
  
  joined_dfs_flagged <- full_join(x = joined_dfs, y = label_flags,
                                  by = c("short_code"))
  
  # Construct variable-unit axis labels
  # x_label <- paste0(x_variable_name,
  #                   " ",
  #                   unique(filter(filtered_data,
  #                                 variable == x_variable_name)$units_label))
  # # y_label <- paste0(y_variable_name,
  #                   " ",
  #                   unique(filter(filtered_data,
  #                                 variable == y_variable_name)$units_label))
  
  # Create the plot with y_var ~ x_var
  two_yr_SWE_plot <-  ggplot(data = joined_dfs_flagged,
                             aes_string(x = make_clean_names(x_variable_name),
                                        y = make_clean_names(y_variable_name),
                                        color = "park_code",
                                        group = "short_code")) +
    geom_point(size = 1) +
    
    geom_text_repel(
      aes(label = short_code),
      data = . %>% filter(flag == 1,
                          year == 2015),
      segment.size = 0.001,
      segment.color = "gray",
      size = 3) +
    geom_text_repel(
      aes(label = short_code),
      data = . %>% filter(flag == 0,
                          year == 2011),
      segment.size = 0.001,
      segment.color = "gray",
      size = 3) +
    geom_line(alpha = 0.3) +
    theme_bw() +
    theme(panel.background = element_blank(),
          legend.position = "none")
  
  # Note transformation if one of the problem vars is y
  if(y_variable_name %in% c("Chlorophyll", "SpCond_top2m", "SO4")) {
    
    two_yr_SWE_plot <- two_yr_SWE_plot +
      scale_y_log10() +
      ylab(y_label)
    
  } else{
    
    two_yr_SWE_plot <- two_yr_SWE_plot +
      ylab(y_label)
    
  }  
  
  if(x_variable_name %in% c("flush_index_SWE_May")) {
    
    two_yr_SWE_plot <- two_yr_SWE_plot +
      scale_x_log10() +
      xlab(x_label)
    
  } else{
    
    two_yr_SWE_plot <- two_yr_SWE_plot +
      xlab(x_label)
    
  }  
  
  
}

#4b. Just one plot at a time --------
vars<-"ProfTemp_top2m"
figs <- map(.x = vars,
            .f = ~ two_yr_line_plot_fun(x_variable_name = "SWE_May",
                                        y_variable_name = .x))

ggsave(filename = "../figures/wtemptop2m_SWE.png",figs[[1]],
       width = 4.3, height = 4, units = "in")

vars<-"ProfTemp_top2m"
figs <- map(.x = vars,
            .f = ~ two_yr_line_plot_fun(x_variable_name = "flush_index_SWE_May",
                                        y_variable_name = .x))

ggsave(filename = "../figures/wtemptop2m_flush_SWE_May.png",figs[[1]],
       width = 4.3, height = 4, units = "in")

# Make gridded plot

# 4c. Water quality variables grid plot -----------------------------------

# Run the make/export function for each variable in bigjoin
vars <- c("DO_top2m", "Chlorophyll", "ProfTemp_top2m", 
                                "secchi_value_m" , "pH_top2m", "SpCond_top2m")

# Plot variable combos vs SWE_May
figs <- map(.x = vars,
                  .f = ~ two_yr_line_plot_fun(x_variable_name = "SWE_May",
                                              y_variable_name = .x))
# Make single gridded plot
grid <- ggarrange(plotlist = figs)
grid

# Plot variable combos vs ice_out_doy
figs2 <- map(.x = vars,
                  .f = ~ two_yr_line_plot_fun(x_variable_name = "ice_out_doy",
                                              y_variable_name = .x))

grid2 <- ggarrange(plotlist = figs2)
grid2

# Plot variable combos vs flush_index_SWE_May
figs3 <- map(.x = vars,
                   .f = ~ two_yr_line_plot_fun(x_variable_name = "flush_index_SWE_May",
                                               y_variable_name = .x))

grid3 <- ggarrange(plotlist = figs3)
grid3

ggsave(filename = "../figures/limno_grid.png",
       plot = grid, width = 9, height = 6, units = "in")
ggsave(filename = "../figures/limno_grid_iceout.png",
       plot = grid2, width = 9, height = 6, units = "in")
ggsave(filename = "../figures/limno_flushindexSWEmay.png",
       plot = grid3, width = 9, height = 6, units = "in")


# 4c. Water chemistry variables grid plot ---------------------------------

# Run the make/export function for each variable in bigjoin
chem_vars <- bigjoin %>%
  dplyr::filter(year %in% c(2011, 2015),
                variable %in% c("Na", "Cl", "Mg", "Ca", "SO4")) %>%
  select(variable) %>%
  unique() %>%
  # Make it a vector
  .$variable


# Plot variable combos vs SWE_May
chem_figs <- map(.x = chem_vars,
                 .f = ~ two_yr_line_plot_fun(x_variable_name = "SWE_May",
                                             y_variable_name = .x))

chem_grid <- ggarrange(plotlist = chem_figs)
chem_grid

# Plot variable combos vs ice_out_doy
chem_figs2 <- map(.x = chem_vars,
                 .f = ~ two_yr_line_plot_fun(x_variable_name = "ice_out_doy",
                                             y_variable_name = .x))

chem_grid2 <- ggarrange(plotlist = chem_figs2)
chem_grid2

# Plot variable combos vs flush_index_SWE_May
chem_figs3 <- map(.x = chem_vars,
                  .f = ~ two_yr_line_plot_fun(x_variable_name = "flush_index_SWE_May",
                                              y_variable_name = .x))

chem_grid3 <- ggarrange(plotlist = chem_figs3)
chem_grid3


ggsave(filename = "../figures/ions_grid.png",
       plot = chem_grid, width = 9, height = 6, units = "in")
ggsave(filename = "../figures/ions_grid_iceout.png",
       plot = chem_grid2, width = 9, height = 6, units = "in")
ggsave(filename = "../figures/ions_flushindexSWEmay.png",
       plot = chem_grid3, width = 9, height = 6, units = "in")


