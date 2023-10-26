
# Read in geodatabase polygon layer
lake_gdb <- st_read(dsn = "./data/NCCN_MtnLakes_ACa02_FCA.gdb",
                    layer = "Lake_Polys")

lake_centroids <- st_centroid(x = lake_gdb) %>%
  filter(NAME != "Gladys annex")%>%
  filter(NAME != "Hoh Lake")%>%
  filter(NAME != "Upper Palisades Lake")

lake_centroids_df<-as.data.frame(lake_centroids)
lake_centroids_df$x_utm<-as.numeric(substr(as.character(lake_centroids_df$Shape),3,17))
lake_centroids_df$y_utm<-as.numeric(substr(as.character(lake_centroids_df$Shape),20,35))
utmcoor<-SpatialPoints(cbind(lake_centroids_df$x_utm,lake_centroids_df$y_utm), proj4string=CRS("+proj=utm +zone=51"))
latlon<-as.data.frame(spTransform(utmcoor,CRS("+proj=longlat")))
latlon$coords.x1<--latlon$coords.x1

lake_centroids_df2<-cbind(lake_centroids_df,latlon)
names(lake_centroids_df2)[which(names(lake_centroids_df2)=="coords.x1")]<-"lon"
names(lake_centroids_df2)[which(names(lake_centroids_df2)=="coords.x2")]<-"lat"

world <- st_read(dsn = "./data/map_files/ne_10m_admin_0_countries.shp") %>%
  filter(NAME %in% c("United States of America", "Canada"))

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

# # Map with polygons, ripe for adding hillshades to...if we can find them
# RAP_polygon_map <- ggplot() +
#   geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
#   geom_sf(data = lake_centroids, inherit.aes = FALSE) +
#   coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
#   ggrepel::geom_text_repel(color="white",
#                            data = lake_centroids,
#                            aes(geometry = Shape, label = short_code),
#                            stat = "sf_coordinates",
#                            min.segment.length = 0) +
#   geom_point(data = RAP_drivers, aes(lon, lat, color = Drivers), inherit.aes = F, size = 1.7)+
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "steelblue1"),
#         panel.grid = element_blank()) +
#   scale_fill_manual(values = c("grey70", "grey40")) +
#   ylab("Latitude") +
#   xlab("Longitude")+
#   labs(title = "Raptorial Rotifer driver TS by lake")
#
# RAP_polygon_map
# ggsave("./figures/Raptorial_map_ts_by_lake.jpg", width = 10, height = 6, units = "in")
#
#
# MICRO_polygon_map <- ggplot() +
#   geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
#   geom_sf(data = lake_centroids, inherit.aes = FALSE) +
#   coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
#   ggrepel::geom_text_repel(color="white",
#                            data = lake_centroids,
#                            aes(geometry = Shape, label = short_code),
#                            stat = "sf_coordinates",
#                            min.segment.length = 0) +
#   geom_point(data = MICRO_drivers, aes(lon, lat, color = Drivers), inherit.aes = F, size = 1.5)+
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "steelblue1"),
#         panel.grid = element_blank()) +
#   scale_fill_manual(values = c("grey70", "grey40")) +
#   ylab("Latitude") +
#   xlab("Longitude")+
#   labs(title = "Microphageous Rotifer driver TS by Lake")
#
# MICRO_polygon_map
# ggsave("./figures/Microfageous_map_ts_by_lake.jpg", width = 10, height = 6, units = "in")
#
# CLAD_polygon_map <- ggplot() +
#   geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
#   geom_sf(data = lake_centroids, inherit.aes = FALSE) +
#   coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
#   ggrepel::geom_text_repel(color="white",
#                            data = lake_centroids,
#                            aes(geometry = Shape, label = short_code),
#                            stat = "sf_coordinates",
#                            min.segment.length = 0) +
#   geom_point(data = CLAD_drivers, aes(lon, lat, color = Drivers), inherit.aes = F, size = 1.5)+
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "steelblue1"),
#         panel.grid = element_blank()) +
#   scale_fill_manual(values = c("grey70", "grey40")) +
#   ylab("Latitude") +
#   xlab("Longitude")+
#   labs(title = "Cladoceran driver TS by lake")
#
# CLAD_polygon_map
# ggsave("./figures/Cladocerans_map_ts_by_lake.jpg", width = 10, height = 6, units = "in")
#
#
# COPE_polygon_map <- ggplot() +
#   geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
#   geom_sf(data = lake_centroids, inherit.aes = FALSE) +
#   coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
#   ggrepel::geom_text_repel(color="white",
#                            data = lake_centroids,
#                            aes(geometry = Shape, label = short_code),
#                            stat = "sf_coordinates",
#                            min.segment.length = 0) +
#   geom_point(data = COPE_drivers, aes(lon, lat, color = Drivers), inherit.aes = F, size = 1.5)+
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "steelblue1"),
#         panel.grid = element_blank()) +
#   scale_fill_manual(values = c("grey70", "grey40")) +
#   ylab("Latitude") +
#   xlab("Longitude")+
#   labs(title = "Copepod driver TS by lake")
#
# COPE_polygon_map
# ggsave("./figures/Copepods_map_ts_by_lake.jpg", width = 10, height = 6, units = "in")
#
#
# ### MAPS BY PARK ###
# RAP_park_polygon_map <- ggplot() +
#   geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
#   geom_sf(data = lake_centroids, inherit.aes = FALSE) +
#   coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
#   ggrepel::geom_text_repel(color="white",
#                            data = lake_centroids,
#                            aes(geometry = Shape, label = short_code),
#                            stat = "sf_coordinates",
#                            min.segment.length = 0) +
#   geom_point(data = RAP_drivers_park, aes(lon, lat, color = Drivers), inherit.aes = F, size = 1.7)+
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "steelblue1"),
#         panel.grid = element_blank()) +
#   scale_fill_manual(values = c("grey70", "grey40")) +
#   ylab("Latitude") +
#   xlab("Longitude")+
#   labs(title = "Raptorial Rotifer driver TS by park")
#
# RAP_park_polygon_map
# ggsave("./figures/Raptorial_map_ts_by_park.jpg", width = 10, height = 6, units = "in")
#
#
# MICRO_park_polygon_map <- ggplot() +
#   geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
#   geom_sf(data = lake_centroids, inherit.aes = FALSE) +
#   coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
#   ggrepel::geom_text_repel(color="white",
#                            data = lake_centroids,
#                            aes(geometry = Shape, label = short_code),
#                            stat = "sf_coordinates",
#                            min.segment.length = 0) +
#   geom_point(data = MICRO_drivers_park, aes(lon, lat, color = Drivers), inherit.aes = F, size = 1.5)+
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "steelblue1"),
#         panel.grid = element_blank()) +
#   scale_fill_manual(values = c("grey70", "grey40")) +
#   ylab("Latitude") +
#   xlab("Longitude")+
#   labs(title = "Microphageous Rotifer driver TS by park")
#
# MICRO_park_polygon_map
# ggsave("./figures/Micro_map_ts_by_park.jpg", width = 10, height = 6, units = "in")
#
#
# CLAD_park_polygon_map <- ggplot() +
#   geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
#   geom_sf(data = lake_centroids, inherit.aes = FALSE) +
#   coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
#   ggrepel::geom_text_repel(color="white",
#                            data = lake_centroids,
#                            aes(geometry = Shape, label = short_code),
#                            stat = "sf_coordinates",
#                            min.segment.length = 0) +
#   geom_point(data = CLAD_drivers_park, aes(lon, lat, color = Drivers), inherit.aes = F, size = 1.5)+
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "steelblue1"),
#         panel.grid = element_blank()) +
#   scale_fill_manual(values = c("grey70", "grey40")) +
#   ylab("Latitude") +
#   xlab("Longitude")+
#   labs(title = "Cladoceran driver TS by park")
#
# CLAD_park_polygon_map
# ggsave("./figures/Cladoceran_map_ts_by_park.jpg", width = 10, height = 6, units = "in")
#
#
# COPE_park_polygon_map <- ggplot() +
#   geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
#   geom_sf(data = lake_centroids, inherit.aes = FALSE) +
#   coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
#   ggrepel::geom_text_repel(color="white",
#                            data = lake_centroids,
#                            aes(geometry = Shape, label = short_code),
#                            stat = "sf_coordinates",
#                            min.segment.length = 0) +
#   geom_point(data = COPE_drivers_park, aes(lon, lat, color = Drivers), inherit.aes = F, size = 1.5)+
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "steelblue1"),
#         panel.grid = element_blank()) +
#   scale_fill_manual(values = c("grey70", "grey40")) +
#   ylab("Latitude") +
#   xlab("Longitude")+
#   labs(title = "Copepod driver TS by park")
#
# COPE_park_polygon_map
# ggsave("./figures/Copepod_map_ts_by_park.jpg", width = 10, height = 6, units = "in")

map_macro_drivers <- map_drivers %>% select(-Elevation_m,
                                            -Depth_max, -fish, -solar_jas) %>%
  melt(., id.vars = c("lon", "lat", "site_code", "park_code"))

map_macro_drivers2 <- map_drivers2 %>% select(-Elevation_m,
                                            -Depth_max, -fish, -solar_jas) %>%
  melt(., id.vars = c("lon", "lat", "site_code", "park_code"))

library(plotly)
p <- ggplot(map_macro_drivers, aes(x = variable, group = value, fill = value)) +
  geom_bar(stat = "count") +
  scale_fill_viridis(discrete = T)+
  facet_wrap(~site_code)
ggplotly(p)
#
library(plotly)
p <- ggplot(map_macro_drivers2, aes(x = variable, group = value, fill = value)) +
  geom_bar(stat = "count") +
  scale_fill_viridis(discrete = T)+
  facet_wrap(~site_code)
ggplotly(p)

# Map with polygons, ripe for adding hillshades to...if we can find them
macro_polygon_map <- ggplot() +
  geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
  geom_sf(data = lake_centroids, inherit.aes = FALSE) +
  coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
  ggrepel::geom_text_repel(color="white",
                           data = lake_centroids,
                           aes(geometry = Shape, label = short_code),
                           stat = "sf_coordinates",
                           min.segment.length = 0) +
  geom_point(data = map_macro_drivers, aes(lon, lat, group = variable, color = value), inherit.aes = F, size = 3)+
  theme_bw() +
  theme(panel.background = element_rect(fill = "steelblue1"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15)) +
  scale_fill_manual(values = c("grey70", "grey40")) +
  ylab("Latitude") +
  xlab("Longitude")+
  facet_wrap(~variable)
macro_polygon_map
ggsave("./figures/macro_map_AR_by_park.jpg", width = 10, height = 10, units = "in")


macro_polygon_map <- ggplot() +
  geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
  geom_sf(data = lake_centroids, inherit.aes = FALSE) +
  coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
  ggrepel::geom_text_repel(color="white",
                           data = lake_centroids,
                           aes(geometry = Shape, label = short_code),
                           stat = "sf_coordinates",
                           min.segment.length = 0) +
  geom_point(data = map_macro_drivers2, aes(lon, lat, group = variable, color = value), inherit.aes = F, size = 3)+
  theme_bw() +
  theme(panel.background = element_rect(fill = "steelblue1"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15)) +
  scale_fill_manual(values = c("grey70", "grey40")) +
  ylab("Latitude") +
  xlab("Longitude")+
  facet_wrap(~variable)
macro_polygon_map
ggsave("./figures/macro_map_AR_by_park_lagged.jpg", width = 10, height = 10, units = "in")


map_zoop_drivers <- zoop_map_drivers %>% select(-Depth_max, -fish, -solar_jas) %>%
  melt(., id.vars = c("lon", "lat", "site_code", "park_code", "Elevation_m"))

map_zoop_drivers2 <- zoop_map_drivers2 %>% select(-Depth_max, -fish, -solar_jas) %>%
  melt(., id.vars = c("lon", "lat", "site_code", "park_code", "Elevation_m"))

p <- ggplot(map_zoop_drivers, aes(x = variable, group = value, fill = value)) +
  geom_bar(stat = "count") +
  scale_fill_viridis(discrete = T)+
  facet_wrap(~site_code)
ggplotly(p)
#
p <- ggplot(map_zoop_drivers2, aes(x = variable, group = value, fill = value)) +
  geom_bar(stat = "count") +
  scale_fill_viridis(discrete = T)+
  facet_wrap(~site_code)
ggplotly(p)

# Map with polygons, ripe for adding hillshades to...if we can find them
zoop_polygon_map <- ggplot() +
  geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
  geom_sf(data = lake_centroids, inherit.aes = FALSE) +
  coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
  ggrepel::geom_text_repel(color="white",
                           data = lake_centroids,
                           aes(geometry = Shape, label = short_code),
                           stat = "sf_coordinates",
                           min.segment.length = 0) +
  geom_point(data = map_zoop_drivers, aes(lon, lat, group = variable, color = value), inherit.aes = F, size = 1.7)+
  theme_bw() +
  theme(panel.background = element_rect(fill = "steelblue1"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15)) +
  scale_fill_manual(values = c("grey70", "grey40")) +
  ylab("Latitude") +
  xlab("Longitude")+
  facet_wrap(~variable)
zoop_polygon_map
ggsave("./figures/zoop_map_AR_by_park.jpg", width = 10, height = 10, units = "in")


zoop_polygon_map <- ggplot() +
  geom_sf(data = world, aes(fill = NAME), color = "black", inherit.aes = F) +
  geom_sf(data = lake_centroids, inherit.aes = FALSE) +
  coord_sf(xlim = c(-124, -120), ylim = c(46.5, 49)) +
  ggrepel::geom_text_repel(color="white",
                           data = lake_centroids,
                           aes(geometry = Shape, label = short_code),
                           stat = "sf_coordinates",
                           min.segment.length = 0) +
  geom_point(data = map_zoop_drivers2, aes(lon, lat, group = variable, color = value), inherit.aes = F, size = 1.7)+
  theme_bw() +
  theme(panel.background = element_rect(fill = "steelblue1"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15)) +
  scale_fill_manual(values = c("grey70", "grey40")) +
  ylab("Latitude") +
  xlab("Longitude")+
  facet_wrap(~variable)
zoop_polygon_map
ggsave("./figures/zoop_map_AR_by_park_lagged.jpg", width = 10, height = 10, units = "in")



 # terrain_map <- openmap(upperLeft = c(49.35, -124.5),
 #                        lowerRight = c(46.5, -120),
 #                        type = 'stamen-terrain',zoom=8)
 #
 #
 # OSM_map <- OpenStreetMap::autoplot.OpenStreetMap(terrain_map) +
 #   geom_sf(size=0,data = st_transform(x = lake_centroids, crs = 3857),
 #           aes(geometry = Shape, alpha=0),  inherit.aes = F) +
 #   labs(caption = "\U00a9 OpenStreetMap contributors") +
 #   xlab("Longitude") +
 #   ylab("Latitude") +
 #   geom_point(data = map_zoop_drivers, aes(lon, lat, group = variable, color = value), inherit.aes = F, size = 1.7)+
 #   ggrepel::geom_text_repel(color="black",segment.size=0.5,box.padding = 0.5,
 #      data = st_transform(x = lake_centroids, crs = 3857),
 #      aes(geometry = Shape, label = short_code), inherit.aes = FALSE,
 #      stat = "sf_coordinates",
 #      min.segment.length = 0) +
 #   theme_bw()+
 #   theme(legend.position = "none")
