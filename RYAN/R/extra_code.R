

plot_test <- ggplot(data = NMDS, aes(MDS1, MDS2)) +
  geom_point(data = nmds_scores, aes(NMDS1, NMDS2, color = site_code ,inherit_aes=F), size = 3)+
  #stat_ellipse(geom = "polygon", aes(group = park_code, color = park_code, fill = park_code), alpha = 0.1) +
  geom_segment(data=vec.sp.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="black",inherit_aes=F) +
  geom_text(data=vec.sp.df,aes(x=MDS1,y=MDS2,label=species),size=3)+
  xlim(c(-1,1.1))+
  ylim(c(-0.6,0.6))+
  theme_bw()

plot_test <- ggplot(data = NMDS, aes(MDS1, MDS2)) +
  geom_point(data = nmds_scores, aes(NMDS1, NMDS2, color = park_code ,inherit_aes=F), size = 3)+
  #stat_ellipse(geom = "polygon", aes(group = park_code, color = park_code, fill = park_code), alpha = 0.1) +
  geom_segment(data=vec.sp.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="black",inherit_aes=F) +
  geom_text(data=vec.sp.df,aes(x=MDS1,y=MDS2,label=species),size=3)+
  xlim(c(-1,1.1))+
  ylim(c(-0.6,0.6))+
  theme_bw()

com = RF_data[,13:16]
m_com = as.matrix(com)

ord = metaMDS(m_com, distance = "bray", trace = F, autotransform = T)
nmds_scores <- cbind(as_tibble(scores(ord)),
                     RF_data[1:2])

NMDS = data.frame(MDS1 = ord$points[,1], MDS2 = ord$points[,2], lake = RF_data[,2], park = RF_data[,1])
simp <- simper(m_com, RF_data$park_code)
summary(simp)
vec.sp<-envfit(ord$points, RF_data[,-c(1,2,3,13,14,15,16)], strata = RF_data$site_code, perm=1000)
vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species<-rownames(vec.sp.df)



# Matrify the DF
zoop <- bind_rows(zoop_no_rotifers, rot_wFFG)%>%
  left_join(x = ., y = match_sites, by = c("Lake" = "old_name"))%>%
  clean_names()%>%
  group_by(park, site_code, lake, year, taxa) %>%
  summarise(density = sum(density)) %>%
  group_by(park, site_code, year) %>%
  mutate(sum_density = sum(density, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_density = density / sum_density) %>%
  filter(prop_density >= .05)%>%
  mutate(park_site_year = paste0(park, "_", lake, "_", year)) %>%
  select(park_site_year, taxa, prop_density) %>%
  data.frame() %>%
  matrify()

zoop_scomm_ids <- rownames_to_column(zoop) %>%
  separate(col = rowname, into = c("park_code", "site_code", "event_year"), sep = "_")%>%
  mutate(event_year = as.character(event_year))

# Determine the proportion
zoop_props <- as.data.frame(zoop) %>%
  bind_cols(park_site_year = row.names(.), .) %>%
  separate(data = ., col = park_site_year, into = c("park", "site", "year"),
           sep = "_") %>%
  gather(key = taxa, value = proportion, -park, -site, -year)

NOCA_zoop_prop_plot <- zoop_props %>%
  filter(park == "NOCA") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site)+
  geom_vline(xintercept = "2015", lwd = 1, color = "white", lty = "dotted")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "inferno")+
  ggtitle(label = "NOCA")

OLYM_zoop_prop_plot <- zoop_props %>%
  filter(park == "OLYM") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site)+
  geom_vline(xintercept = "2015", lwd = 1, color = "white", lty = "dotted")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "inferno")+
  ggtitle(label = "OLYM")

MORA_zoop_prop_plot <- zoop_props %>%
  filter(park == "MORA") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site)+
  geom_vline(xintercept = "2015", lwd = 1, color = "white", lty = "dotted")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "inferno")+
  ggtitle(label = "MORA")

zoop_proportions <- (OLYM_zoop_prop_plot+MORA_zoop_prop_plot)/
  (NOCA_zoop_prop_plot+plot_spacer())
zoop_proportions

ggsave(path = ".", filename = "./figures/Zooplankton Proportions.jpg",
       width = 20, height = 12, device='jpg', dpi=500)

##############################
# BIND ENV DATA WITH ZOOP DATA
##############################

data <- read_csv("./data/dat_temp.csv")%>%
  mutate(event_year = as.character(event_year))

envs_zoops <- left_join(data, zoop_scomm_ids,
                        by = c("park_code","site_code","event_year"))

vis_dat(x = envs_zoops)


## ---------------------------------------------------------------------
# Down some rows after removing NAs...
envs_zoops <- na.omit(envs_zoops)

corrplot(cor(select(envs_zoops, BotTemp:RAP)), type = "upper",
         tl.col = "black", tl.srt = 45)

subset_nmds <- metaMDS(comm = select(envs_zoops, CLAD:RAP),
                       distance = "bray", k = 3, trymax = 100)
# ## ----echo=FALSE-------------------------------------------------------
# subset_nmds
#
# plot(subset_nmds)
#
# nmds_scores_subset <- cbind(as_tibble(scores(subset_nmds)),
#                             select(envs_zoops, park_code:event_year))
#
# library(plotly)
# fig <- plot_ly(nmds_scores_subset, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~park_code, colors = c('#BF382A', '#0C4B8E','green'))
# fig <- fig %>% add_markers()
# fig

