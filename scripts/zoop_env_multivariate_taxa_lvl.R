## ---- message=FALSE---------------------------------------------------

library(tidyverse)
library(readxl)
library(vegan)
library(labdsv)
#library(kableExtra)
library(viridis)
library(visdat)
library(corrplot)
library(psych)
library(ggvegan)
library(janitor)
library(ggpubr)



## ---------------------------------------------------------------------
bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")
zoop <- read_excel("../data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")

#  Prep data
zoop[1487, "Taxa"] <- "ROT"
zoop[1487, "Taxonid"] <- 5

# Name matching table
match_sites <- readRDS(file = file.path("..",
                                        "data",
                                        "name_site_matches.rds"))

# Crosswalk common names for sites to site_codes
zoop <- left_join(x = zoop, y = match_sites,
                  by = c("Lake" = "old_name")) %>%
  clean_names()

# Remove copepodites and nauplii from the dataset per MM suggestion
zoop_orig <- zoop

zoop <- zoop %>%
  filter(!grepl(pattern = "copepodite", x = gen_sp),
         !grepl(pattern = "nauplii", x = gen_sp),
         taxa != "INS", taxa != "HAR")


## ---------------------------------------------------------------------
zoop_prop_taxa <- zoop %>%
  # Dropping Code for now b/c unclear what it means. Code != Taxa though.
  # Also dropping taxonid
  group_by(park, site_code, lake, year, taxa) %>%
  summarise(density = sum(density)) %>%
  group_by(park, site_code, year) %>%
  mutate(sum_density = sum(density, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_density = density / sum_density) %>%
  filter(prop_density >= .05)

# Add a row identifier column, turn into matrix
zoop_comm_matrix <- zoop_prop_taxa %>%
  mutate(park_site_year = paste0(park, "_", site_code, "_", year)) %>%
  select(park_site_year, taxa, prop_density) %>%
  data.frame() %>%
  matrify()


## ---------------------------------------------------------------------
head(zoop_comm_matrix)
sum(zoop_comm_matrix[1, ])



## ----results="hide"---------------------------------------------------
# Adapted from https://ourcodingclub.github.io/2018/05/04/ordination.html
scree_plot <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10),
       replicate(10, metaMDS(comm = x, distance = "bray", k = 1)$stress),
       xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions",
       ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x,  distance = "bray", k = i + 1)$stress))
  }
}

scree_plot(zoop_comm_matrix)


## ----results="hide"---------------------------------------------------
nmds_2 <- metaMDS(comm = zoop_comm_matrix, distance = "bray", k = 2, trymax = 100)


## ---------------------------------------------------------------------
nmds_2


## ----echo=FALSE-------------------------------------------------------
# Park/site/year data
identity_data <- rownames_to_column(zoop_comm_matrix) %>%
  separate(col = rowname, into = c("park", "site", "year"), sep = "_") %>%
  select(park:year)

# Combine identity data with scores
nmds_scores <- cbind(as_tibble(scores(nmds_2)),
                     identity_data)

species_scores <- rownames_to_column(as.data.frame(scores(nmds_2, "species"))) %>%
  rename(taxa = rowname)

park_nmds_plot <- ggplot() +
  # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = taxa),
  #           alpha = 0.5, size = 7) +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2, 
                                     fill = park), size = 3, pch = 21) +
  scale_fill_manual(values = magma(15)[c(3, 8, 11)],
                    name = "Park") +
  annotate(geom = "label", x = -0.30, y = 1.25, size = 5,
           label = paste("Stress: ", round(nmds_2$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(size = 24))

year_nmds_plot <- ggplot() +
  # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = taxa),
  #           alpha = 0.5, size = 7) +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2, 
                                     fill = year), size = 3, pch = 21) +
  scale_fill_manual(values = viridis(11),
                    name = "Year") +
  annotate(geom = "label", x = -0.35, y = 1.25, size = 5,
           label = paste("Stress: ", round(nmds_2$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(size = 24))

site_nmds_plot <- ggplot() +
  # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = taxa),
  #           alpha = 0.5, size = 7) +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2, 
                                     fill = site), size = 3, pch = 21) +
  scale_fill_manual(values = plasma(21),
                    name = "Site") +
  annotate(geom = "label", x = 0, y = 1.25, size = 5,
           label = paste("Stress: ", round(nmds_2$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(size = 24))

# Plot the figs to rmd knit
park_nmds_plot
year_nmds_plot
site_nmds_plot

three_nmds_plots <- ggarrange(plotlist = list(park_nmds_plot,
                                              year_nmds_plot,
                                              # Reduce legend size for arranging:
                                              site_nmds_plot +
                                                theme(text = element_text(size = 18))),
                              ncol = 2, nrow = 2, widths = c(.5,.5,1)) %>%
  annotate_figure(top = text_grob("Zoop NMDS plots colored by Park/Year/Site",
                                  face = "bold", size = 16))

ggsave(filename = "../figures/multivariate/zoop_combined_nmds.png",
       plot = three_nmds_plots, device = "png", width = 10, height = 8,
       units = "in")



## ---------------------------------------------------------------------
zoop_scomm_ids <- rownames_to_column(zoop_comm_matrix) %>%
  separate(col = rowname, into = c("park", "site", "year"), sep = "_")

zoop_permanova <- adonis(formula = select(zoop_scomm_ids, CLAD:ROT) ~ year + site + park,
                         data = zoop_scomm_ids, method = "bray")

zoop_permanova



## ---------------------------------------------------------------------
# Reshape the proportion community data into a useful data frame for ggplot2
zoop_props <- as.data.frame(zoop_comm_matrix) %>%
  bind_cols(park_site_year = row.names(.), .) %>%
  separate(data = ., col = park_site_year, into = c("park", "site", "year"),
           sep = "_") %>%
  gather(key = taxa, value = proportion, -park, -site, -year)

noca_zoop_uni_plot <- zoop_props %>%
  filter(park == "NOCA") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "bottom") +
  scale_fill_viridis_d(option = "inferno") 


## ----echo = FALSE-----------------------------------------------------
noca_zoop_uni_plot +
  ggtitle(label = "NOCA zoop relative proportions by lake and year")



## ---------------------------------------------------------------------
mora_zoop_uni_plot <- zoop_props %>%
  filter(park == "MORA") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "bottom") +
  scale_fill_viridis_d(option = "inferno") 


## ----echo = FALSE-----------------------------------------------------
mora_zoop_uni_plot +
  ggtitle(label = "MORA zoop relative proportions by lake and year")



## ---------------------------------------------------------------------
olym_zoop_uni_plot <- zoop_props %>%
  filter(park == "OLYM") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "bottom") +
  scale_fill_viridis_d(option = "inferno") 


## ----echo = FALSE-----------------------------------------------------
olym_zoop_uni_plot +
  ggtitle(label = "OLYM zoop relative proportions by lake and year")



## ----echo = FALSE-----------------------------------------------------
zoop_uni_plots <- ggarrange(noca_zoop_uni_plot, mora_zoop_uni_plot,
                            olym_zoop_uni_plot, labels = c("NOCA", "MORA", "OLYM"),
                            # ncol = 2, nrow = 2,
                            common.legend = TRUE, legend = "right",
                            label.y = 1, label.x = -0.05,
                            font.label = list(size = 12)) %>%
  annotate_figure(top = text_grob("Zoop order relative props by park and lake",
                                  face = "bold", size = 16))

ggsave(filename = "../figures/multivariate/zoop_combined_props_plot.png",
       plot = zoop_uni_plots, device = "png", width = 10, height = 8,
       units = "in")


## ---------------------------------------------------------------------
zoop_scomm_ids <- zoop_scomm_ids %>%
  mutate(year = as.numeric(year))

yearly_bigjoin <- bigjoin %>%
  filter(variable %in% c("secchi_value_m", "AirTemp", "BotTemp",
                         "MidTemp", "SurfTemp", "DO_top2m")) %>%
  select(park_code, site_code, event_year, variable, value) %>%
  unique() %>%
  spread(key = variable, value = value)

# Join the zoops to yearly_bigjoin bc some data will be NA: both datasets
# should have corresponding rows
yearly_bigjoin_zoop <- inner_join(x = zoop_scomm_ids, y = yearly_bigjoin,
                                  by = c("park" = "park_code",
                                         "site" = "site_code",
                                         "year" = "event_year"))

vis_dat(x = yearly_bigjoin_zoop)


## ---------------------------------------------------------------------
# Down some rows after removing NAs...
yearly_bigjoin_zoop <- na.omit(yearly_bigjoin_zoop)

corrplot(cor(select(yearly_bigjoin_zoop, AirTemp:SurfTemp)), type = "upper", 
         tl.col = "black", tl.srt = 45)

# corr.test(x = select(yearly_bigjoin_zoop, AirTemp:SurfaceTemp))


## ---------------------------------------------------------------------
corrplot(cor(select(yearly_bigjoin_zoop, SurfTemp, DO_top2m, secchi_value_m)),
         type = "upper", tl.col = "black", tl.srt = 45)

corr.test(x = select(yearly_bigjoin_zoop, SurfTemp, DO_top2m, secchi_value_m))



## ---------------------------------------------------------------------

zoop_dbrda <- dbrda(formula = select(yearly_bigjoin_zoop, CLAD:ROT) ~
                      SurfTemp + DO_top2m + secchi_value_m, data = yearly_bigjoin_zoop,
                    distance = "bray")

zoop_dbrda

autoplot(zoop_dbrda)


## ----echo = FALSE-----------------------------------------------------
ggsave(filename = "../figures/multivariate/zoop_dbrda.png",
       plot = autoplot(zoop_dbrda) +
         ggtitle("Distance-based RDA for zoop community"),
       device = "png", width = 10, height = 6,
       units = "in")


## ---------------------------------------------------------------------
# https://sites.ualberta.ca/~ahamann/teaching/renr690/Lab9b.pdf
anova(zoop_dbrda)
# anova(zoop_dbrda, by = "axis", perm.max = 999) # Won't run bc negative eigenvalues
anova(zoop_dbrda, by = "terms", perm.max = 999)


## ----results="hide"---------------------------------------------------
subset_nmds <- metaMDS(comm = select(yearly_bigjoin_zoop, CLAD:ROT),
                       distance = "bray", k = 2, trymax = 100)


## ----echo=FALSE-------------------------------------------------------
subset_nmds

# Combine identity data with scores
nmds_scores_subset <- cbind(as_tibble(scores(subset_nmds)),
                            select(yearly_bigjoin_zoop, park:year))

park_nmds_subset_plot <- ggplot() +
  # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = taxa),
  #           alpha = 0.5, size = 7) +
  geom_point(data = nmds_scores_subset, aes(x = NMDS1, y = NMDS2, 
                                            fill = park), size = 3, pch = 21) +
  scale_fill_manual(values = inferno(15)[c(3, 8, 11)],
                    name = "Park") +
  annotate(geom = "label", x = -0.05, y = 1.25, size = 7,
           label = paste("Stress: ", round(subset_nmds$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 24))

year_nmds_subset_plot <- ggplot() +
  # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = taxa),
  #           alpha = 0.5, size = 7) +
  geom_point(data = nmds_scores_subset, aes(x = NMDS1, y = NMDS2, 
                                            fill = as.factor(year)), size = 3, pch = 21) +
  scale_fill_manual(values = inferno(11),
                    name = "Year") +
  annotate(geom = "label", x = -0.25, y = 1.25, size = 7,
           label = paste("Stress: ", round(subset_nmds$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 24))

site_nmds_subset_plot <- ggplot() +
  # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = taxa),
  #           alpha = 0.5, size = 7) +
  geom_point(data = nmds_scores_subset, aes(x = NMDS1, y = NMDS2, 
                                            fill = site), size = 3, pch = 21) +
  scale_fill_manual(values = magma(20),
                    name = "Site") +
  annotate(geom = "label", x = 0.05, y = 1.25, size = 7,
           label = paste("Stress: ", round(subset_nmds$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 24))

three_nmds_subset_plots <- ggarrange(park_nmds_subset_plot, year_nmds_subset_plot,
                                     site_nmds_subset_plot) %>%
  annotate_figure(top = text_grob("Zoop NMDS plots on subsetted data, colored by Park/Year/Site",
                                  face = "bold", size = 16))

ggsave(filename = "../figures/multivariate/zoop_combined_subset_nmds.png",
       plot = three_nmds_subset_plots, device = "png", width = 10, height = 8,
       units = "in")

