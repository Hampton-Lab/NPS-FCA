library(sf)
library(tidyverse)
library(tigris)
library(ggrepel)
library(OpenStreetMap)
library(rgdal)

# 1. Load in datasets -----------------------------------------------------

# Read in geodatabase polygon layer
lake_gdb <- st_read(dsn = "../data/NCCN_MtnLakes_ACa02_FCA.gdb",
                    layer = "Lake_Polys")

lake_centroids <- st_centroid(x = lake_gdb) %>%
  filter(NAME != "Gladys annex")

lake_centroids_df<-as.data.frame(lake_centroids)
lake_centroids_df$x_utm<-as.numeric(substr(as.character(lake_centroids_df$Shape),3,17))
lake_centroids_df$y_utm<-as.numeric(substr(as.character(lake_centroids_df$Shape),20,35))
utmcoor<-SpatialPoints(cbind(lake_centroids_df$x_utm,lake_centroids_df$y_utm), proj4string=CRS("+proj=utm +zone=51"))
latlon<-as.data.frame(spTransform(utmcoor,CRS("+proj=longlat")))
latlon$coords.x1<--latlon$coords.x1

lake_centroids_df2<-cbind(lake_centroids_df,latlon)
names(lake_centroids_df2)[which(names(lake_centroids_df2)=="coords.x1")]<-"lon"
names(lake_centroids_df2)[which(names(lake_centroids_df2)=="coords.x2")]<-"lat"


world <- st_read(dsn = "../data/map_files/ne_10m_admin_0_countries.shp") %>%
  filter(NAME %in% c("United States of America", "Canada"))


# 2. lake codes -----------------------------------------------------------------

short_codes <- tribble(
  ~NAME, ~short_code,
  "Upper Palisades Lake",     "PA",
  "LH15",     "15",
  "Lake Allen",     "AL",
  "LP19",     "19",
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
  "Crazy Lake ",     "CR",
  "Milk Lake",     "MI",
  "Lake LaCrosse",     "LC",
  "Lake Sunup",     "SU",
  "Lake Connie",     "CO",
  "Hoh Lake",     "HO"
)

lake_centroids <- full_join(x = lake_centroids, y = short_codes, by = c("NAME"))
lake_centroids2<-full_join(x = lake_centroids_df2, y = short_codes, by = c("NAME"))

# 3. Maps -----------------------------------------------------------------

# 3.1 Map set up for hillshades (polygon-based) ---------------------------

# Map with polygons, ripe for adding hillshades to...if we can find them
polygon_map <- ggplot() +
  geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
  geom_sf(data = lake_centroids, inherit.aes = FALSE) +
  coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
  # Made possible with https://github.com/slowkow/ggrepel/issues/111
  ggrepel::geom_text_repel(color="white",
    data = lake_centroids,
    aes(geometry = Shape, label = short_code),
    stat = "sf_coordinates",
    min.segment.length = 0) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "steelblue1"),
        panel.grid = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("grey70", "grey40")) +
  ylab("Latitude") +
  xlab("Longitude")

ggsave(filename = "../figures/shapefile_map.png",
       plot = polygon_map, device = "png", width = 8, height = 7, units = "in")


# 3.2 Map using a terrain layer pulled from OSM ---------------------------

# Note that there are some usage requirements, which we should be clear on prior
# to publishing

# Options: https://www.r-bloggers.com/the-openstreetmap-package-opens-up/
terrain_map <- openmap(upperLeft = c(49.35, -124.5),
                       lowerRight = c(46.5, -120),
                       type = 'stamen-terrain',zoom=8)
#terrain_map <- openmap(upperLeft = c(50, -125),
#                       lowerRight = c(45.5, -119),
#                       type = 'stamen-terrain')

OSM_map <- autoplot(terrain_map) +
  geom_sf(size=10,data = st_transform(x = lake_centroids, crs = 3857),
          aes(geometry = Shape, alpha=0),  inherit.aes = F) +
  labs(caption = "\U00a9 OpenStreetMap contributors") +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_text(size=3,data = st_transform(x = lake_centroids, crs = 3857),
            aes(geometry = Shape, label = short_code,color=ParkCode), inherit.aes = FALSE,
            stat = "sf_coordinates")+
#  ggrepel::geom_text_repel(color="white",segment.size=0,box.padding = 0.5,
#    data = st_transform(x = lake_centroids, crs = 3857),
#    aes(geometry = Shape, label = short_code), inherit.aes = FALSE,
#    stat = "sf_coordinates",
#    min.segment.length = 0) +
  theme_bw()+
  theme(legend.position = "none")

ggsave(filename = "../figures/osm_map.png",
       plot = OSM_map, device = "png", width = 6, height = 6, units = "in")

do<-0
if(do==1){
test<-leaflet(lake_centroids3) %>%
#  setView(lng = lon_chosen, lat = lat_chosen, zoom = zoomstart)  %>% #setting the view over ~ center of North America
  fitBounds(lat1=46.5,lat2=49.5,
            lng1=-124.5,lng2=-120) %>%
  addTiles(urlTemplate="https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png") %>%   
  addLabelOnlyMarkers(lng=lake_centroids3$lon,lat=lake_centroids3$lat)+
  geom_point(data=lake_centroids3,aes(x=lon,y=lat))

}