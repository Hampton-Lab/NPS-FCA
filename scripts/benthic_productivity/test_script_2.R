################################################################
## Script to calculate benthic primary production              #
#  based on lake morphology and certain water characteristics  #
################################################################

#[X] want day length in MINUTES!!!!
#had been in hours, adjusting to minutes to match 15 min increments for time
#ok, can do...

#ahhhhh....should be time increment as 0.25, as in quarter hours...
# [ ] try this!

#set working directory
setwd("D:/Labou/Benthic_Production")

#load libraries
library(xlsx)
library(reshape2)
library(dplyr)

## ============== read in data ================ ##

dat_samples <- read.xlsx("template_example_data.xlsx", sheetName = "sample")

dat_morpho <- read.xlsx("template_example_data.xlsx", sheetName = "morpho")

# get depth slices and change in surface area for morpho data
dat_morpho_slices <- dat_morpho %>% 
                      mutate(benthic_area_z = lag(depth_surface_area) - (depth_surface_area))
#yes - at 1 m depth, now have X m of benthic area, and so forth downward

#get day length in minutes
dat_samples <- dat_samples %>% mutate(day_length_mins = day_length*60)


## ============== loop for calculation for SINGLE LAKE ================ ##

dat_samples
dat_morpho_slices

#here, we are assuming that kd, benthic_ik, and benthic_pmax do NOT vary with depth
#(or at least, that no one will have data to that effect)
#trying this first, THEN can worry about if people have depth-varying data

# i = day
# j = depth 
# k = time
# so BPPR is some value for time k, at depth j, on day i

# SET UP OUTPUT DATA FRAME
count_days <- length(dat_samples$day_length_mins)
count_depths <- (length(dat_morpho_slices$depth_morpho) - 1)
#because first depth is surface, which has no benthic area
#have benthic depths as number depths minus 1

count_total <- count_days*count_depths #*count_times

#data frame with lakename, dmy, depth, and BPPR_z
#number of days * number of depths = total rows count
outputs <- data.frame(lake_name = character(count_total),
                      dmy_i = integer(count_total), 
                      depth_j = integer(count_total),
                      #time_k = numeric(count_total),
                      BPPR_z = numeric(count_total))

outputs$lake_name <- as.character(outputs$lake_name)

#assign lake to outputs dataframe
outputs[1:length(outputs$lake_name), 1] <- unique(as.character(dat_samples$lake_name))

#LAKE LEVEL INFO
#total benthic area for LAKE (stays constant across days and time)
benthic_area_total <- sum(dat_morpho_slices$benthic_area_z, na.rm = TRUE)

#=================================================================================
#START FOR LOOP THAT GOES THROUGH EACH *DAY* (FOR A SINGLE LAKE)
#GIVES DAILY BENTHIC PRIMARY PRODUCTION PER DAY
#highest step: loop through days, so start with day 1
for (i in 1:length(dat_samples$day_length_mins)) {
  
  #put day into outputs dataframe
  if (i==1) {
    benthic_dat <- dat_samples[i,]
    outputs[i:(length(dat_morpho_slices$depth_morpho)-1), 2] <- paste(benthic_dat$day, benthic_dat$month, benthic_dat$year, sep = "-")
    #length-1 because not counting at zero depth
    #ok, a bit complex:
    #want to start at i=1 and run length of depths
    #for next one, want to start after that ends and run same depth
    #so start at length + 1 and run to i*length
    #example: i=2 starts at 11 and runs to 20
  } else if (i==2) {
  benthic_dat <- dat_samples[i,]
  outputs[((i-1)+(length(dat_morpho_slices$depth_morpho)-1)):((length(dat_morpho_slices$depth_morpho)-1)*i), 2] <- paste(benthic_dat$day, benthic_dat$month, benthic_dat$year, sep = "-")
  } else {
  benthic_dat <- dat_samples[i,]
  outputs[(1+((length(dat_morpho_slices$depth_morpho)-1)*(i-1))):((length(dat_morpho_slices$depth_morpho)-1)*i), 2] <- paste(benthic_dat$day, benthic_dat$month, benthic_dat$year, sep = "-")
  }

  #===============================================================================
  #START FOR LOOP THAT GOES THROUGH EACH *DEPTH*
  #GIVES DAILY AVERAGE BPPR FOR EACH DEPTH
  #at some depth z
  for (j in 2:length(dat_morpho_slices$depth_morpho)){ #start at 2 so depth of 1m (since at surface have technically no benthic surface)
    morpho_dat <- dat_morpho_slices[j,]
    
    #put depth into outputs dataframe
    #this is a work of ART
    if (i==1) {
      outputs[(j-1), 3] <- morpho_dat$depth_morpho #j-1 because starts at j=2, but want to fill in row starting at 1
    } else if (i==2) {
      outputs[((i-1)+(length(dat_morpho_slices$depth_morpho)-1) + (j-2)), 3] <- morpho_dat$depth_morpho
    } else {
      outputs[((1+((length(dat_morpho_slices$depth_morpho)-1)*(i-1))) + (j-2)), 3] <- morpho_dat$depth_morpho
    }

    #time is in MINUTES - loop through day at some depth, repeat for each depth
    t_incs <- seq(0, (benthic_dat$day_length_mins), 15) 
    
    #=============================================================================
    #START FOR LOOP THAT GOES THROUGH ENTIRE DAY IN INTERVALS FOR DEPTH
    #GIVES DAILY AVERAGE BPPR AT DEPTH Z
    #BPPRz (average BPP at depth z)
    for (k in 1:length(t_incs)) {
      #set time
      t <- t_incs[k]
      #surface light at time
      surface_light_t <- benthic_dat$noon_surface_light * sin(pi*(t/benthic_dat$day_length_mins))
      
      #surface light at time at depth
      depth_light_z_t <- surface_light_t * (exp(-1* (benthic_dat$light_atten_coeff) * (morpho_dat$depth_morpho)))
      
      #NOTE: very low levels of light WILL cause issues down the road with being numeric(0) not working with df
      #attempt to catch these...
      test_zero <- numeric(0)
      if(identical(test_zero, depth_light_z_t) == TRUE) {
        depth_light_z_t <- 0
      } else {
        depth_light_z_t <- depth_light_z_t
      }
      
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
      outputs[((i-1)+(length(dat_morpho_slices$depth_morpho)-1) + (j-2)), 4] <- BPPR_z
    } else {
      outputs[((1+((length(dat_morpho_slices$depth_morpho)-1)*(i-1))) + (j-2)), 4] <- BPPR_z
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

#ok, the values look ok
#BPPR_zs look in range, although first depth is high
#overall total is lower than expected, but then again - these values are kind of out of back pocket

#should use vanebancour data to test - see if get ballpark same results
#(since this is a cross between vadenbancour and that thesis...)










