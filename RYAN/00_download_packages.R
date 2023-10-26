##################
# REQUIRE PACKAGES
##################

set.seed(1)
if (!require("ggvegan")) remotes::install_github("gavinsimpson/ggvegan", force = T)
if (!require("OpenStreetMap")) install.packages("OpenStreetMap")
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,zoo,vegan,readxl,lme4,OpenStreetMap,
               viridis,patchwork,ggpubr,visdat,labdsv,psych,
               reshape2,ggvegan,janitor,MuMIn,kableExtra,
               sf,tigris,ggrepel,osmdata,rgdal,ggmap,corrplot,
               MCMCvis, lubridate,tidybayes,ncdf4,imputeTS,devtools,
               scales,rjags,coda,R2jags,gridExtra,PerformanceAnalytics,
               broom.mixed, plotrix)
