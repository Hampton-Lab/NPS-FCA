library(sf)
library(tidyverse)
library(tigris)
library(ggrepel)
library(OpenStreetMap)
library(rgdal)
library(readxl)
library(gridExtra)

# Generate lake and park maps

# Warning: Strangely, this script is not 100% reliable and sometimes produced psychadelic graphics anomalies on SP mac. 
# So if errors happen, try rerunning. 

# 1. Load in datasets -----------------------------------------------------

# Read in geodatabase polygon layer
lake_gdb <- st_read(dsn = "../data/NCCN_MtnLakes_ACa02_FCA.gdb", layer = "Lake_Polys")

# Administrative Boundaries of National Park System Units 9/30/2019 - National Geospatial Data Asset (NGDA) NPS National Parks Dataset
# https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True
parkspoly <- st_read(dsn = "../data/parkspoly/nps_boundaries.shp")

world <- st_read(dsn = "../data/map_files/ne_10m_admin_0_countries.shp") %>%
  filter(NAME %in% c("United States of America", "Canada"))

physio <- read_excel(
  path = file.path("..","data","analysis_outputs", "study-site-tables.xlsx")) %>% as.data.frame()

# 2. lake codes -----------------------------------------------------------------
short_codes <- tribble(
  ~Lake, ~short_code,
  "Upper Palisades Lake",     "PA",
  "Lake LH15",     "15",
  "Lake Allen",     "AL",
  "Lake LP19",     "19",
  "Upper Deadwood Lake",     "DW",
  "Blue Lake",     "BL",
  "Lower Blum Lake",     "LB",
  "Lower Silent Lake",     "SI",
  "Easy Ridge Lake",     "ER",
  "Lower East Lake",     "LE",
  "Bowan Lake",     "BO",
  "Upper Triplet Lake",     "TR",
  "Gladys Lake",     "GL",
  "Ferry Lake",     "FE",
  "Heather Lake",     "HE",
  "Crazy Lake",     "CR",
  "Milk Lake",     "MI",
  "Lake La Crosse",     "LC",
  "Lake Sunup",     "SU",
  "Lake Connie",     "CO",
  "Hoh Lake",     "HO"
)

physio <- full_join(x = physio, y = short_codes, by = c("Lake"))
physio$lat[which(physio$Lake=="Lake LH15")]<-0.03+physio$lat[which(physio$Lake=="Lake LH15")]
physio$lon[which(physio$Lake=="Lake LH15")]<--0.03+physio$lon[which(physio$Lake=="Lake LH15")]
#physio$lat[which(physio$Lake=="Upper Palisades Lake")]<--0.02+physio$lat[which(physio$Lake=="Upper Palisades Lake")]
#physio$lon[which(physio$Lake=="Upper Palisades Lake")]<-0.02+physio$lon[which(physio$Lake=="Upper Palisades Lake")]

##### 3. Make a region-level map with park boundaries shown ################

# get ggplot's colors for park boundaries using scale_color_manual()
df<-data.frame(x=c(1:3),y=4:6,group=c("A","B","C"))
g<-ggplot_build(ggplot(df,aes(x,y,fill=group))+geom_point())
unique(g$data[[1]]["fill"])
#fill
#r 1 #F8766D
#g 2 #00BA38
#b 3 #619CFF

# the regional map 
region_map_parks <- ggplot() +
  geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
  geom_sf(data = parkspoly,aes(fill=UNIT_CODE))+
  geom_text(color="black",size=3,
            data = parkspoly,
            aes(geometry = geometry, label = UNIT_CODE),
            stat = "sf_coordinates",
            nudge_y=-0.5,
            nudge_x=-0.2)+
  coord_sf(xlim = c(-126, -118), ylim = c(46, 50)) +  
  theme_bw() +
  theme(panel.background = element_rect(fill = "gray60"),
        panel.grid = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("white", "#F8766D","#00BA38","#619CFF","white"))+#,#F8766D","#00BA38","#619CFF")) +
  annotate(geom="text",label = 'Canada', x = Inf, y = Inf, vjust = 2, hjust=2,size=2.5)+
  annotate(geom="text",label = 'USA', x = Inf, y = -Inf, vjust = -4,hjust=2,size=2.5)+
  annotate(geom="text",label = 'Pacific', x = -Inf, y = -Inf, vjust = -2,hjust=-0.5,size=2.5)+  
  annotate(geom="text",label = 'Seattle', x = -121.8, y = 47.65,size=2.5)+  
  
  ylab("Latitude") +
  xlab("Longitude")

# or, an alternate region-level map without park boundaries (good luck trying to add polygons to an OpenStreetMap)
region_map0 <- openmap(upperLeft = c(49.5, -124.5),
                       lowerRight = c(46.5, -121),
                       type = 'stamen-terrain',zoom=7)
region_map <- OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(region_map0)) +
  geom_sf(data = st_transform(x = parkspoly, crs = 3857),
          aes(geometry=geometry,alpha=0.5),  inherit.aes = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()+ 
  theme(legend.position = "none")

##### Make park-level maps with lake sites as points ################
# See options: https://www.r-bloggers.com/the-openstreetmap-package-opens-up/

latlon_adj<-0.2
#latlon_adj<-0.35
latlon_adj2<-0

mora_map0 <- openmap(upperLeft = c(47.4+latlon_adj2, -122.3-latlon_adj2),
                    lowerRight = c(46.4-latlon_adj2, -121.2+latlon_adj2),
                    type = 'stamen-terrain',zoom=9)
noca_map0 <- openmap(upperLeft = c(49.2+latlon_adj, -121.785-latlon_adj),
                    lowerRight = c(48.2-latlon_adj, -120.685+latlon_adj),
                    type = 'stamen-terrain',zoom=9)
#noca_map0 <- openmap(upperLeft = c(49+latlon_adj, -121.6-latlon_adj),
#                     lowerRight = c(48-latlon_adj, -120.5+latlon_adj),
#                     type = 'stamen-terrain',zoom=9)
olym_map0 <- openmap(upperLeft = c(48.3+latlon_adj2, -123.9-latlon_adj2),
                     lowerRight = c(47.3-latlon_adj2, -122.8+latlon_adj2),
                     type = 'stamen-terrain',zoom=9)

mora_map<-OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(mora_map0)) +
  geom_point(data = datai<-physio %>% filter(Park_code=="MORA"), 
             size=6,
             alpha=0.5,
             color="#F8766D",
             aes(x = lon, y = lat))+
  geom_text(data = datai<-physio %>% filter(Park_code=="MORA"), 
            size=3,
            color="black",
            aes(x = lon, y = lat,label=short_code))+
  xlab("")+ylab("")+
#  annotate(geom="text",label = 'MORA', x = Inf, y = -Inf,hjust=-1,vjust=1)+  
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin=unit(c(0,0.1,0,0), "cm"))

noca_map<-OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(noca_map0)) +
  geom_point(data = physio %>% filter(Park_code=="NOCA"), 
             size=6,
             alpha=0.5,
             color="#00BA38",
             aes( x = lon, y = lat))+
  geom_text(data = datai<-physio %>% filter(Park_code=="NOCA"), 
            size=3,
            color="black",
            aes( x = lon, y = lat,label=short_code))+
  xlab("")+ylab("")+
#  annotate(geom="text",label = 'NOCA', x = Inf, y = Inf,hjust=-1,vjust=-1)+  
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin=unit(c(0,0.1,0,0), "cm"))

olym_map<-OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(olym_map0)) +
  geom_point(data = physio %>% filter(Park_code=="OLYM"), 
             size=6,
             alpha=0.5,
             color="#619CFF",
             aes( x = lon, y = lat))+
  geom_text(data = datai<-physio %>% filter(Park_code=="OLYM"), 
            size=3,
            color="black",
            aes( x = lon, y = lat,label=short_code))+
  xlab("")+ylab("")+
#  annotate(geom="text",label = 'OLYM', x = -Inf, y = -Inf,hjust=1,vjust=1)+  
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin=unit(c(0,0.1,0,0), "cm"))


##### 4. Combine maps ################

multimap<-grid.arrange(region_map_parks,
                       noca_map,
                       olym_map,
                       mora_map,
                       nrow=2,
                       padding=0)

ggsave(filename = "../figures/multimap.png",
       plot = multimap, device = "png", width = 6, height = 6, units = "in")

