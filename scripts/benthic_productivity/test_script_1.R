setwd("D:/Labou/Benthic_Production")

library(xlsx)
library(reshape2)
library(dplyr)

#dat_stable <- read.xlsx("test_data.xlsx", sheetName = "stable")
#dat_varies <- read.xlsx("test_data.xlsx", sheetName = "varies")
dat_morpho <- read.xlsx("test_data.xlsx", sheetName = "morpho")

#dat_stable #incld. kd, benthic and phyto kd, and benthic pmax
#dat_varies #day length, chla_depth, chla_biomass
dat_morpho #morpho depths and surface area slices

dat_combined <- read.xlsx("test_data.xlsx", sheetName = "combined")

dat_combined #combines stable and variable (so stable just repeats)
dat_morpho

## get depth slices and change in surface area for morpho data
dat_morpho_slices <- dat_morpho %>% 
                      mutate(slice = paste(depth_slice, lead(depth_slice), sep = "_"),
                             benthic_area_z = lag(water_surface_area_slice) - (water_surface_area_slice))
dat_morpho_slices

# ## equations ##
# 
# #find increments (15 mins, for day length)
# t_incs <- seq(0, (60*day_length), 15) #time is in MINUTES
# 
# for (i in 1:length(t_incs)) {
#   t <- t_incs[i]
# }
# 
# #maximum primary productivity (pelagic)
# ppMax <- 2.2(chla_biomass)
# 
# #surface light at time t (from above)
# surface_light_t <- noon_surface_light * sin(pi*(t/day_length))
# 
# #light at depth z, time t (stays at depth, time varies)
# depth_light_z_t <- surface_light_t * (e^(-light_attenuation_coefficient)*depth_slice)
# 
# #BPPR at depth z, time t (stays at depth, time varies)
# BPPR_zt <- benthic_pmax * tanh(depth_light_z_t/benthic_ik)
# 
# #BPPRz (average BPP at depth z)
# t_incs <- seq(0, (60*day_length), 15) #time is in MINUTES
# BPPR_z_pt1 <- for (i in 1:length(t_incs)) {
#                 #set time
#                 t <- t_incs[i]
#                 #surface light at time
#                 surface_light_t <- noon_surface_light * sin(pi*(t/day_length))
#                 #surface light at time at depth
#                 depth_light_z_t <- surface_light_t * (e^(-light_attenuation_coefficient)*depth_slice)
#                 #BPPR at time and depth
#                 BPPR_zt <- benthic_pmax * tanh(depth_light_z_t/benthic_ik)
#                 #sum up BPPR_zt to get total BPPR z over day
#                 #prelim tests indicate that this type of structure works for this
#                 if (i == 1) {
#                   BPPR_z_day <- BPPR_zt
#                 } else {
#                   BPPR_z_day <- BPPR_z_day + BPPR_zt
#                 }
#             }
#               
# BPPR_z_pt2 <- ((BPPR_z_pt1)/(1/15))*(benthic_area_z/benthic_area_total) 
# #where benthic_area_total = sum of all benthic_area_z
# 
# BPPR_total <- sum(BPPR_z_pt2)/benthic_area_total
# 
# #max benthic production (at depth z)- base calc, run over time
# 
# #optional assign:
# # bpmax = 5 | 50 | 120 
# # Ik(phyto) = 180, Ik(benthic) = 300
# 
# # for (i in 1:length(dat_morpho_slices$depth_slice)) {
# #   dat_z <- dat_morpho_slices[c(i-1, 1),]
# # }
# 
# # BPz <- bpmax * tanh(depth_light_t/benthic_ik)*((lag(dat_z$water_surface_area) - dat_z$watwer_surface_area_slice))
# #bpmax * tanh * (light at depth, time/benthic sat) * (change in surface area between above and this depth)
# 
# # find increments (15 mins, for day length)
# t_incs <- seq(0, (60*day_length), 15) #time is in MINUTES
# 
# for (i in 1:length(t_incs)) {
#   t <- t_incs[i]
# }
# 
# BPz <- bpmax * tanh(depth_light_t/benthic_ik)*(benthic_area_z)
# 
# # at depth z, iterate over time
# 
# for (i in 1:length(t_incs)) {
#   t <- t_incs[i]
#   
# }

#make this a function - provide depth, etc.

## test with combined data ##
#using 1 m depth intervals
#using 15 min time intervals

###### bringing it ALL together (she says optimistically) #######
dat_combined
dat_morpho_slices

#here, we are assuming that kd, benthic_ik, and benthic_pmax do NOT vary with depth
#(or at least, that no one will have data to that effect)
#trying this first, THEN can worry about if people have depth-varying data

# i = day
# j = depth 
# k = time
# so BPPR is some value for time k, at depth j, on day i

#SET UP OUTPUT DATA FRAME
count_days <- length(dat_combined$day_length)
count_depths <- (length(dat_morpho_slices$depth_slice) - 1)
#count_times <- length(seq(0, 60*(max(dat_combined$day_length)), 15))

count_total <- count_days*count_depths #*count_times

outputs <- data.frame(lake_name = character(count_total),
                      dmy_i = integer(count_total), 
                      depth_j = integer(count_total),
                      #time_k = numeric(count_total),
                      BPPR_z = numeric(count_total))

outputs$lake_name <- as.character(outputs$lake_name)

#assign lake to outputs dataframe
outputs[1:length(outputs$lake_name), 1] <- unique(as.character(dat_combined$lake_name))

#LAKE LEVEL INFO
#total benthic area for LAKE (stays constant across days and time)
benthic_area_total <- sum(dat_morpho_slices$benthic_area_z, na.rm = TRUE)

#=================================================================================
#START FOR LOOP THAT GOES THROUGH EACH *DAY*
#GIVES DAILY BENTHIC PRIMARY PRODUCTION PER DAY
#highest step: loop through days, so start with day 1
for (i in 1:length(dat_combined$day_length)) {
  
  #put day into outputs dataframe
  if (i==1) {
    benthic_dat <- dat_combined[i,]
    outputs[i:(length(dat_morpho_slices$depth_slice)-1), 2] <- paste(benthic_dat$day, benthic_dat$month, benthic_dat$year, sep = "-")
    #length-1 because not counting at zero depth
    #ok, a bit complex:
    #want to start at i=1 and run length of depths
    #for next one, want to start after that ends and run same depth
    #so start at length + 1 and run to i*length
    #example: i=2 starts at 11 and runs to 20
  } else if (i==2) {
  benthic_dat <- dat_combined[i,]
  outputs[(i-1)+(length(dat_morpho_slices$depth_slice)-1):((length(dat_morpho_slices$depth_slice)-1)*i), 2] <- paste(benthic_dat$day, benthic_dat$month, benthic_dat$year, sep = "-")
  } else {
  benthic_dat <- dat_combined[i,]
  outputs[(1+((length(dat_morpho_slices$depth_slice)-1)*(i-1))):((length(dat_morpho_slices$depth_slice)-1)*i), 2] <- paste(benthic_dat$day, benthic_dat$month, benthic_dat$year, sep = "-")
  }

  #===============================================================================
  #START FOR LOOP THAT GOES THROUGH EACH *DEPTH*
  #GIVES DAILY AVERAGE BPPR FOR EACH DEPTH
  #at some depth z
  for (j in 2:length(dat_morpho_slices$depth_slice)){ #start at 2 so depth of 1m (since at surface have technically no benthic surface)
    morpho_dat <- dat_morpho_slices[j,]
    
    #put depth into outputs dataframe
    #this is a work of ART
    if (i==1) {
      outputs[(j-1), 3] <- morpho_dat$depth_slice #j-1 because starts at j=2, but want to fill in row starting at 1
    } else if (i==2) {
      outputs[((i-1)+(length(dat_morpho_slices$depth_slice)-1) + (j-2)), 3] <- morpho_dat$depth_slice
    } else {
      outputs[((1+((length(dat_morpho_slices$depth_slice)-1)*(i-1))) + (j-2)), 3] <- morpho_dat$depth_slice
    }

    #time is in MINUTES - loop through day at some depth, repeat for each depth
    t_incs <- seq(0, (60*benthic_dat$day_length), 15) 
    
    #=============================================================================
    #START FOR LOOP THAT GOES THROUGH ENTIRE DAY IN INTERVALS FOR DEPTH
    #GIVES DAILY AVERAGE BPPR AT DEPTH Z
    #BPPRz (average BPP at depth z)
    for (k in 1:length(t_incs)) {
      #set time
      t <- t_incs[k]
      #surface light at time
      #NOTE: very low levels of light WILL cause issues down the road with being numeric(0) not working with df
      surface_light_t <- benthic_dat$noon_surface_light * sin(pi*(t/benthic_dat$day_length))
      #surface light at time at depth
      depth_light_z_t <- surface_light_t *(exp(-1* (benthic_dat$light_attenuation_coefficient) * (morpho_dat$depth_slice)) )
      #BPPR at time and depth
      BPPR_zt <- benthic_dat$benthic_pmax * tanh(depth_light_z_t/benthic_dat$benthic_ik)
      #sum up BPPR_zt to get total BPPR z over day
      #prelim tests indicate that this type of structure works for this
      if (k == 1) {
        BPPR_zt_total <- BPPR_zt
      } else {
        BPPR_zt_total <- BPPR_zt_total + BPPR_zt
      }
    }
    
    #finish BPPRz calc (to get average at depth z)
    BPPR_z <- ((BPPR_zt_total)/(1/15))*((morpho_dat$benthic_area_z)/benthic_area_total) 
    #where benthic_area_total = sum of all benthic_area_z
    
    #put in outputs dataframe
# >>>>>>>>>>>>>>>>>>>>>> this needs work to put outputs in correct place...
    if (i==1) {
      outputs[(j-1), 4] <- BPPR_z #j-1 because starts at j=2, but want to fill in row starting at 1
    } else if (i==2) {
      outputs[((i-1)+(length(dat_morpho_slices$depth_slice)-1) + (j-2)), 4] <- BPPR_z
    } else {
      outputs[((1+((length(dat_morpho_slices$depth_slice)-1)*(i-1))) + (j-2)), 4] <- BPPR_z
    }
  }
  
}

outputs

#calculate total benthic pr
BPPR_total <- sum(outputs$BPPR_z)/benthic_area_total
#think per time at depth should be in df
#and then goes into per depth

BPPR_total

#the code puts out a value, needs some work OUTSIDE the nested for loops
#to make sure that BPPR_total is calculated properly

#because this has...negative production










