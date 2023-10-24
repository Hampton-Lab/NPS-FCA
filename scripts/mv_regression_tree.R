# If you need to install mvpart, you can uncomment and run the below code

# url <- "https://cran.r-project.org/src/contrib/Archive/mvpart/mvpart_1.6-2.tar.gz"
# pkgFile <- "mvpart_1.6-2.tar.gzz"
# download.file(url = url, destfile = pkgFile)
# 
# # Install package
# install.packages(pkgs=pkgFile, type="source", repos=NULL)
# 
# # Delete package tarball
# unlink(pkgFile)


library(mvpart)
library(tidyverse)
library(vegan)
library(ggrepel)

zoop_calcyc_tree_data <- read.csv(file = "../data/analysis_outputs/zoop_tree_data_calcyc_w_env.csv")

head(zoop_calcyc_tree_data)

zoop_calcyc_tree_data_no_na <- zoop_calcyc_tree_data %>%
  filter(!is.na(calanoid), 
         !is.na(cladoceran),
         !is.na(cyclopoid),
         !is.na(rotifer),
         !is.na(chlorophyll),
         !is.na(surf_temp),
         !is.na(ice_out_doy),
         !is.na(ice_in_doy),
         !is.na(ph_top2m),
         !is.na(air_temp),
         !is.na(po4)) %>%
  mutate(total_copep = calanoid + cyclopoid)

calccyc_nmds <- metaMDS(log10(zoop_calcyc_tree_data_no_na[ , c(101, 103, 104, 105)]+1), distance = "bray", try = 100)

log_species <- log10(zoop_calcyc_tree_data_no_na[ , c(101, 103, 104, 105)]+1)

zoop_calcyc_rda <- capscale(log_species ~ surf_temp + ice_out_doy + ph_top2m + chlorophyll + k + ca + mg,
                         data = zoop_calcyc_tree_data_no_na, distance = "bray")

anova(zoop_calcyc_rda, permutations = 999, by = "margin")
anova(zoop_calcyc_rda, permutations = 999, by = "axis")


# Pull scores from NMDS and add site data
data_scores <- as.data.frame(scores(x = zoop_calcyc_rda, display = "sites", scaling = "sites"))
data_scores$ice_out_doy <- zoop_calcyc_tree_data_no_na$ice_out_doy
data_scores$ph_top2m <- zoop_calcyc_tree_data_no_na$ph_top2m
data_scores$chlorophyll <- zoop_calcyc_tree_data_no_na$chlorophyll


# Pull species scores from NMDS
species_scores <- as.data.frame(scores(x = zoop_calcyc_rda, display = "species", scaling = "none"))
species_scores$species <- rownames(species_scores)

# Pull arrow values 
biplot_scores <- as.data.frame(scores(x = zoop_calcyc_rda, display = "bp"))
biplot_scores$env_var <- rownames(biplot_scores)

# Plot the capscale
zoop_capscale <- ggplot() +
  geom_point(data = data_scores,
             aes(x = CAP1, y = CAP2)) +
  geom_text(data = species_scores , 
                  aes(x = CAP1, y = CAP2, label = species)) + 
  geom_segment(data = biplot_scores, 
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.5, "cm"))) + 
  geom_text_repel(data = biplot_scores , 
            aes(x = CAP1, y = CAP2, label = env_var), color = "purple") + 
  #annotate("label", x = -0.35, y = -0.75, size = 10,
  #         label = paste("Stress: ", round(calccyc_nmds$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key=element_blank(),
        text = element_text(size = 24))

ggplot(zoop_calcyc_tree_data_no_na  %>%
         filter(cyclopoid > 0,
                calanoid > 0), 
       aes((log10(rotifer+1)), log10(calanoid+1)/log10(cyclopoid+1))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(zoop_calcyc_tree_data_no_na  %>%
         filter(cyclopoid > 0,
                calanoid > 0), 
       aes((surf_temp), log10(calanoid+1))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(zoop_calcyc_tree_data_no_na  %>%
         filter(cyclopoid > 0,
                calanoid > 0), 
       aes((log10(rotifer+1)), log10(cyclopoid+1))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(zoop_calcyc_tree_data_no_na  %>%
         filter(cyclopoid > 0,
                calanoid > 0), 
       aes((surf_temp), log10(rotifer+1))) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(log10(calanoid+1)/log10(cyclopoid+1) ~ (surf_temp), 
           data = zoop_calcyc_tree_data_no_na  %>%
             filter(cyclopoid > 0,
                    calanoid > 0)))

ggplot(zoop_calcyc_tree_data_no_na  %>%
         filter(cyclopoid > 0,
                calanoid > 0), 
       aes((ice_out_doy), log10(cyclopoid+1)/log10(calanoid+1))) +
  geom_point() +
  geom_smooth(method = "lm")

ggsave("../figures/zooplankton_nmds.png", plot = zoop_nmds, device = "png", width = 12, height = 10, units = "in")

png(file="../figures/zooplankton_dbrda.png", width = 12, height = 10, units = "in", res = 300)
ordiplot(zoop_calcyc_rda)
dev.off()

ordiplot(zoop_calcyc_rda)

anova(object = zoop_calcyc_rda, permutations = 999, by = "margin")

uni_tree <- mvpart(as.matrix(cbind(log10((zoop_calcyc_tree_data$calanoid+1)/(zoop_calcyc_tree_data$cyclopoid+1)), 
                                   #log10(zoop_calcyc_tree_data$rotifer+1), 
                                   log10(zoop_calcyc_tree_data$cladoceran+1))) ~ ca + chlorophyll + air_temp + ice_out_doy, data = zoop_calcyc_tree_data)


uni_tree <- mvpart(as.matrix(log10(zoop_calcyc_tree_data[ , 100:103]+1)) ~ chlorophyll + surf_temp + log10(rotifer+1), data = zoop_calcyc_tree_data)

uni_tree_prune <- prune(uni_tree, cp =0.02)
plot(uni_tree_prune, compress = TRUE)
text(uni_tree_prune, use.n = TRUE)

mv_regression <- mvpart(as.matrix(log10(zoop_tree_data[ , c(80:82)]+1)) ~
                          chlorophyll + air_temp + ca, data = zoop_tree_data)


# What if we remove all NAs first? MRB included this code as a test against
# the results of using sklearn.tree.DecisionTreeRegressor() in Python. The
# two match if you use sklearn.tree.DecisionTreeRegressor(max_depth = 1)
no_na_data <- select(zoop_tree_data, clad:rot, chlorophyll, air_temp, ca, park_code)[complete.cases(select(zoop_tree_data, clad:rot, chlorophyll, air_temp, ca)),]

mv_regression <- mvpart(as.matrix(log10(no_na_data[ , c(1:3)]+1)) ~
                          chlorophyll + air_temp + ca, data = no_na_data)


# Analysis of rotifer community in response to cyclopoids

zoop_genus <- read.csv(file = "../data/analysis_outputs/zoop_tree_data_genera_w_env.csv")

summary(zoop_genus)

zoop_genus_combined <- zoop_genus %>%
  mutate(total_cyclops = Macrocyclops + Microcyclops + Diacyclops,
         total_cyclops = log10(total_cyclops+1),
         total_calanoid = Hesperodiaptomus + Leptodiaptomus + Onychodiaptomus,
         total_calanoid = log10(total_calanoid+1)) %>%
  select(park_code, site_code, event_year, Polyarthra, Synchaeta, 
         Kellicottia, Keratella, 
         Philodina, Trichocerca, Ascomorpha, Monostyla, Lecane, Collotheca, Conochilus,
         Trichotria, Conochiloides, Notholca, Brachionus, Filinia, Ploesoma, Alona, 
         total_cyclops, Polyphemus, surf_temp, total_calanoid) %>%
  rowwise() %>%
  mutate(sum_rotifer = log10(sum(c_across(Polyarthra:Alona), na.rm = TRUE)+1)) %>%
  filter(total_cyclops != 0,
         total_calanoid != 0,
         sum_rotifer != 0) %>%
  drop_na()
  # gather(Rotifer, Count, Conochilus:Alona) %>%
  # mutate(Count = log10(Count+1)) %>%
  # spread(Rotifer, Count) %>%
  # drop_na() 

all_rotifers_lm <- lm(total_calanoid/total_cyclops ~ (sum_rotifer), zoop_genus_combined)

summary(all_rotifers_lm)

all_rot_plot <- ggplot(zoop_genus_combined, aes(sum_rotifer, 
                                                total_calanoid/total_cyclops)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  ylab("total_calanoid/total_cyclops") +
  xlab("Total Rotifers (Log transformed)") +
  ggtitle("All Rotifer Genera Considered") +
  annotate(geom = "label", x = 3.35, y = 2.6,
           label = paste0("p-value: ",
                          round(summary(all_rotifers_lm)$coefficients[2, 4], 4),
                          "\nR-squared: ",
                          round(summary(all_rotifers_lm)$r.squared, 3))) +
  theme_minimal()

ggplot(zoop_genus_combined %>%
         # pivot_longer(cols = c(Polyarthra, Synchaeta, Keratella, Kellicottia), 
         #              names_to = "rotifer_genus", values_to = "counts"), 
         mutate(spiney_rotifer = Kellicottia + Keratella,
                soft_rotifer = Polyarthra + Synchaeta),
       aes(log10(spiney_rotifer+1), total_calanoid/total_cyclops)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  ylab("Total Cyclopoids (Log transformed)") +
  xlab("Total Rotifers (Log transformed)") +
  ggtitle("All Rotifer Genera Considered") +
  #facet_wrap(~rotifer_type) +
  theme_minimal()

ggsave(filename = "../figures/all_rotifers_vs_cyclopods.png", plot = all_rot_plot,
       device = "png", width = 8, height = 6, units = "in")

## residuals and normality actually look pretty good! 
plot(all_rotifers_lm)

# Analysis of rotifer community in response to cyclopoids

zoop_genus_combined <- zoop_genus %>%
  mutate(total_cyclops = Macrocyclops + Microcyclops + Diacyclops,
         total_cyclops = log10(total_cyclops+1)) %>%
  select(park_code, site_code, event_year, Polyarthra, Synchaeta, 
         Conochilus, 
         #Kellicottia, Keratella, 
         Philodina, Trichocerca, Ascomorpha, Monostyla, Lecane, Collotheca,
         Trichotria, Conochiloides, Notholca, Brachionus, Filinia, Ploesoma, Alona, 
         total_cyclops, Polyphemus) %>%
  rowwise() %>%
  mutate(sum_rotifer = log10(sum(c_across(Polyarthra:Alona), na.rm = TRUE)+1)) %>%
  filter(total_cyclops != 0,
         sum_rotifer != 0) %>%
  drop_na()
# gather(Rotifer, Count, Conochilus:Alona) %>%
# mutate(Count = log10(Count+1)) %>%
# spread(Rotifer, Count) %>%
# drop_na() 


no_spiney_rotifers_lm <- lm(total_cyclops ~ (sum_rotifer), zoop_genus_combined)

summary(no_spiney_rotifers_lm)

ns_rot_plot <- ggplot(zoop_genus_combined, aes((sum_rotifer), (total_cyclops))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  ylab("Total Cyclopoids (Log transformed)") +
  xlab("Total Rotifers (Log transformed)") +
  ggtitle("All Rotifer Genera (Except Keratella and Kellicotia) Considered") +
  annotate(geom = "label", x = 3.35, y = 2.6,
           label = paste0("p-value: ",
                          round(summary(no_spiney_rotifers_lm)$coefficients[2, 4], 4),
                          "\nR-squared: ",
                          round(summary(no_spiney_rotifers_lm)$r.squared, 3))) +
  theme_minimal()

ggsave(filename = "../figures/no_spiney_rotifers_vs_cyclopods.png", plot = ns_rot_plot,
       device = "png", width = 8, height = 6, units = "in")

## residuals and normality actually look pretty good! 
plot(no_spiney_rotifers_lm)

# Analysis of rotifer community in response to cyclopoids

zoop_genus_combined <- zoop_genus %>%
  mutate(total_cyclops = Macrocyclops + Microcyclops + Diacyclops,
         total_cyclops = log10(total_cyclops+1)) %>%
  select(park_code, site_code, event_year, Polyarthra, Synchaeta, 
         #Conochilus, 
         #Kellicottia, Keratella, 
         #Philodina, Trichocerca, Ascomorpha, Monostyla, Lecane, Collotheca,
         #Trichotria, Conochiloides, Notholca, Brachionus, Filinia, Ploesoma, Alona, 
         total_cyclops, Polyphemus) %>%
  rowwise() %>%
  mutate(sum_rotifer = log10(sum(c_across(Polyarthra:Synchaeta), na.rm = TRUE)+1)) %>%
  filter(total_cyclops != 0,
         sum_rotifer != 0) %>%
  drop_na()
# gather(Rotifer, Count, Conochilus:Alona) %>%
# mutate(Count = log10(Count+1)) %>%
# spread(Rotifer, Count) %>%
# drop_na() 

poly_syn_rotifers_lm <- lm(total_cyclops ~ (sum_rotifer), zoop_genus_combined)

summary(poly_syn_rotifers_lm)

poly_syn_rot_plot <- ggplot(zoop_genus_combined, aes((sum_rotifer), (total_cyclops))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  ylab("Total Cyclopoids (Log transformed)") +
  xlab("Total Rotifers (Log transformed)") +
  ggtitle("Polyarthra & Synchaeta Considered") +
  annotate(geom = "label", x = 3.35, y = 2.6,
           label = paste0("p-value: ",
                          round(summary(poly_syn_rotifers_lm)$coefficients[2, 4], 4),
                          "\nR-squared: ",
                          round(summary(poly_syn_rotifers_lm)$r.squared, 3))) +
  theme_minimal()

ggsave(filename = "../figures/poly_syn_rotifers_vs_cyclopods.png", plot = poly_syn_rot_plot,
       device = "png", width = 8, height = 6, units = "in")

## residuals and normality actually look pretty good! 
plot(poly_syn_rotifers_lm)


### Multivariate rotifer

zoop_genus_mv <- zoop_genus %>%
  mutate(total_cyclops = Macrocyclops + Microcyclops + Diacyclops,
         total_cyclops = log10(total_cyclops+1)) %>%
  select(park_code, site_code, event_year, Polyarthra, Synchaeta, 
         Kellicottia, Keratella, 
         #Philodina, Trichocerca, Ascomorpha, Monostyla, Lecane, Collotheca,
         #Trichotria, Conochiloides, Notholca, Brachionus, Filinia, Ploesoma, Alona, 
         total_cyclops, Polyphemus, chlorophyll, surf_temp) %>%
  rowwise() %>%
  mutate(sum_rotifer = (sum(c_across(Polyarthra:Keratella), na.rm = TRUE))) %>%
  filter(total_cyclops != 0,
         sum_rotifer != 0) %>%
  drop_na()

rotifer_nmds <- metaMDS(comm = log10(zoop_genus_mv[ , 4:7]+1), distance = "bray")

adonis2(formula = log10(zoop_genus_mv[ , 4:7]+1) ~ surf_temp * total_cyclops * chlorophyll, data = zoop_genus_mv,
        method = "bray")

# Pull scores from NMDS and add site data
data_scores <- as.data.frame(scores(x = rotifer_nmds, display = "sites"))
data_scores$total_cyclops <- zoop_genus_mv$total_cyclops
data_scores$surf_temp <- zoop_genus_mv$surf_temp

species_scores <- as.data.frame(scores(x = rotifer_nmds, display = "species"))
species_scores$species <- rownames(species_scores)

# Plot NMDS
rotifer_nmds_plot <- ggplot() +
  geom_point(data = data_scores,
             aes(x = NMDS1, y = NMDS2, size = total_cyclops, color = surf_temp)) +
  #scale_size_continuous(range = c(12, 28), guide = FALSE) +
  #scale_color_manual(values = inferno(15)[c(3, 8, 11, 14)],
  #                   name = "IDW Population Grouping") +
  #guides(colour = guide_legend(override.aes = list(size = 10))) +
  geom_text_repel(data = species_scores,
                  aes(x = NMDS1, y = NMDS2, label = species)) + 
  coord_equal() +
  annotate("label", x = -0.15, y = 0.4, size = 4,
           label = paste("Stress: ",
                         round(rotifer_nmds$stress, digits = 3))) +
  theme(legend.position = "right",
        legend.key=element_blank(),
        strip.text.x = element_text(size = 20, color = "grey80"),
        text = element_text(size = 24),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        panel.background = element_rect("white"),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80"),
        axis.ticks = element_line(color = "grey80"))

rotifer_nmds_plot

ggsave(filename = "../figures/rotifer_nmds_plot.png",
       plot = rotifer_nmds_plot, device = "png",
       height = 6, width = 12, dpi = 300)


rotifer_dbrda <- capscale(formula = log10(zoop_genus_mv[ , 4:7]+1) ~ surf_temp + total_cyclops + chlorophyll, data = zoop_genus_mv,
                       method = "bray")

ordiplot(rotifer_dbrda)

### Calanoid to Cylopoid ratios 

zoop_gensp_tree_data <- read.csv(file = "../data/analysis_outputs/zoop_tree_data_genera_w_env.csv")

head(zoop_gensp_tree_data)

zoop_genus_mv <- zoop_genus %>%
  mutate(total_cyclops = Macrocyclops + Microcyclops + Diacyclops,
         #total_cyclops = log10(total_cyclops+1),
         total_calanoids = Hesperodiaptomus + Leptodiaptomus + Onychodiaptomus,
         #total_calanoids = log10(total_calanoids+1),
         cal_cyc_ratio = total_cyclops/total_calanoids) %>%
  select(park_code, site_code, event_year, Polyarthra, Synchaeta, 
         Kellicottia, Keratella, 
         #Philodina, Trichocerca, Ascomorpha, Monostyla, Lecane, Collotheca,
         #Trichotria, Conochiloides, Notholca, Brachionus, Filinia, Ploesoma, Alona, 
         total_cyclops, Polyphemus, chlorophyll, surf_temp, total_calanoids, 
         cal_cyc_ratio, ice_out_doy, total_p, ca ) %>%
  rowwise() %>%
  #mutate(sum_rotifer = (sum(c_across(Polyarthra:Keratella), na.rm = TRUE))) %>%
  #filter(is.finite(cal_cyc_ratio),
  #       cal_cyc_ratio > 0) %>%
  drop_na()

ggplot(zoop_genus_mv, aes(ice_out_doy, log10(total_cyclops+1))) +
  geom_point()


zoop_genus_capscale <- capscale(log10(zoop_genus_mv[ , 4:7]+1) ~ log10(total_cyclops+1) + (ice_out_doy) + surf_temp, data = zoop_genus_mv)

# Pull scores and add site data
data_scores <- as.data.frame(scores(x = zoop_genus_capscale, display = "sites", scaling = "sites"))
data_scores$ice_out_doy <- zoop_genus_mv$ice_out_doy

# Pull species scores S
species_scores <- as.data.frame(scores(x = zoop_genus_capscale, display = "species", scaling = "none"))
species_scores$species <- rownames(species_scores)

# Pull arrow values 
biplot_scores <- as.data.frame(scores(x = zoop_genus_capscale, display = "bp"))
biplot_scores$env_var <- rownames(biplot_scores)

# Plot the capscale
zoop_capscale <- ggplot() +
  geom_point(data = data_scores,
             aes(x = CAP1, y = CAP2)) +
  geom_text(data = species_scores , 
            aes(x = CAP1, y = CAP2, label = species)) + 
  geom_segment(data = biplot_scores, 
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.5, "cm"))) + 
  geom_text_repel(data = biplot_scores , 
                  aes(x = CAP1, y = CAP2, label = env_var), color = "purple") + 
  #annotate("label", x = -0.35, y = -0.75, size = 10,
  #         label = paste("Stress: ", round(calccyc_nmds$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key=element_blank(),
        text = element_text(size = 24))

anova(object = zoop_genus_capscale, permutations = 999, by = "margin")


ggplot(data = zoop_calcyc_tree_data, aes(ph_top2m, chlorophyll)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()


# Macroinvertebrate mobility analyses -------------------------

macro_mobility <- read.csv("../data/analysis_outputs/macro_tree_data_mobility_w_env.csv")

macro_mobility_full <- macro_mobility %>%
  filter(!is.na(chlorophyll),
         !is.na(surf_temp),
         !is.na(ice_out_doy),
         #!is.na(ice_in_doy),
         !is.na(ph_top2m),
         !is.na(air_temp),
         !is.na(do_top2m),
         !is.na(ca))

mobility_rda <- capscale(log10(macro_mobility_full[ , 99:101]+1) ~ ice_out_doy + mg + ca + do_top2m + surf_temp + chlorophyll + ph_top2m,
                      data = macro_mobility_full, distance = "bray")
mobility_rda
plot(mobility_rda)


# Pull scores and add site data
data_scores <- as.data.frame(scores(x = mobility_rda, display = "sites", scaling = "sites"))

# Pull species scores S
species_scores <- as.data.frame(scores(x = mobility_rda, display = "species", scaling = "none"))
species_scores$species <- rownames(species_scores)

# Pull arrow values 
biplot_scores <- as.data.frame(scores(x = mobility_rda, display = "bp"))
biplot_scores$env_var <- rownames(biplot_scores)

# Plot the capscale
mobility_capscale <- ggplot() +
  geom_point(data = data_scores,
             aes(x = CAP1, y = CAP2)) +
  geom_text(data = species_scores , 
            aes(x = CAP1, y = CAP2, label = species), size = 10) + 
  geom_segment(data = biplot_scores, 
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.5, "cm"))) + 
  geom_text_repel(data = biplot_scores , 
                  aes(x = CAP1, y = CAP2, label = env_var), color = "purple") + 
  #annotate("label", x = -0.35, y = -0.75, size = 10,
  #         label = paste("Stress: ", round(calccyc_nmds$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key=element_blank(),
        text = element_text(size = 24))

anova(object = mobility_rda, permutations = 999, by = "margin")

macro_mobility_full %>%
  pivot_longer(cols = c(fa, sa, s), names_to = "mobility", values_to = "counts") %>%
ggplot() +
  geom_point(aes(ice_out_doy, log10(counts+1))) +
  facet_wrap(~mobility) +
  geom_smooth(aes(ice_out_doy, log10(counts+1)), method = "lm") +
  theme_bw()

macro_mobility_full %>%
  pivot_longer(cols = c(fa, sa, s), names_to = "mobility", values_to = "counts") %>%
  filter(do_top2m >= 5) %>%
  ggplot() +
  geom_point(aes(do_top2m, log10(counts+1))) +
  facet_wrap(~mobility) +
  geom_smooth(aes(do_top2m, log10(counts+1)), method = "lm") +
  theme_bw()


summary(lm(log10(counts+1) ~ ice_out_doy,
           data = macro_mobility_full %>%
             pivot_longer(cols = c(fa, sa, s), names_to = "mobility", values_to = "counts") %>%
             filter(mobility == "fa")))

macro_mobility %>%
  select(park_code, site_code, event_year, 
         fa, s, sa) %>%
  pivot_longer(cols = c(fa, sa, s), names_to = "mobility", values_to = "counts") %>%
  group_by(park_code, site_code, event_year) %>%
  mutate(total_invert = sum(counts),
         prop_invert = counts/total_invert) %>%
  ggplot(aes(x = event_year, y = prop_invert, fill = mobility)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "viridis") +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~site_code) +
  theme_bw()

# Macroinvertebrate feeding  -------------------------

macro_feeding <- read.csv("../data/analysis_outputs/macro_tree_data_feeding_w_env.csv")

macro_feeding_full <- macro_feeding %>%
  filter(!is.na(chlorophyll),
         !is.na(surf_temp),
         !is.na(ice_out_doy),
         #!is.na(ice_in_doy),
         !is.na(ph_top2m),
         !is.na(air_temp),
         !is.na(do_top2m),
         !is.na(ca))

feeding_rda <- capscale(log10(macro_feeding_full[ , 99:103]+1) ~ ice_out_doy + ca + mg + do_top2m + surf_temp + chlorophyll,
                         data = macro_feeding_full, distance = "bray")
feeding_rda
plot(feeding_rda)


# Pull scores and add site data
data_scores <- as.data.frame(scores(x = feeding_rda, display = "sites", scaling = "sites"))

# Pull species scores S
species_scores <- as.data.frame(scores(x = feeding_rda, display = "species", scaling = "none"))
species_scores$species <- rownames(species_scores)

# Pull arrow values 
biplot_scores <- as.data.frame(scores(x = feeding_rda, display = "bp"))
biplot_scores$env_var <- rownames(biplot_scores)

# Plot the capscale
feeding_capscale <- ggplot() +
  geom_point(data = data_scores,
             aes(x = CAP1, y = CAP2)) +
  geom_text(data = species_scores , 
            aes(x = CAP1, y = CAP2, label = species)) + 
  geom_segment(data = biplot_scores, 
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.5, "cm"))) + 
  geom_text_repel(data = biplot_scores , 
                  aes(x = CAP1, y = CAP2, label = env_var), color = "purple") + 
  #annotate("label", x = -0.35, y = -0.75, size = 10,
  #         label = paste("Stress: ", round(calccyc_nmds$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key=element_blank(),
        text = element_text(size = 24))

anova(object = feeding_rda, permutations = 999, by = "margin")

macro_feeding_full %>%
  filter(do_top2m >= 5) %>%
  pivot_longer(cols = c(filter_feeder, unlabeled, predator, gathering_collector, shredder_herbivore), 
               names_to = "feeding_style", values_to = "counts") %>%
  ggplot() +
  geom_point(aes(do_top2m, log10(counts+1))) +
  facet_wrap(~feeding_style) +
  geom_smooth(aes(do_top2m, log10(counts+1)), method = "lm") +
  theme_bw()

summary(lm(log10(counts+1) ~ (ice_out_doy) + ca,
           data = macro_feeding_full %>%
             pivot_longer(cols = c(filter_feeder, unlabeled, predator, gathering_collector, shredder_herbivore), 
                          names_to = "feeding_style", values_to = "counts") %>%
             filter(feeding_style == "shredder_herbivore")))


macro_feeding_full %>%
  pivot_longer(cols = c(filter_feeder, unlabeled, predator, gathering_collector, shredder_herbivore), 
               names_to = "feeding_style", values_to = "counts") %>%
  ggplot() +
  geom_point(aes(mg, log10(counts+1))) +
  facet_wrap(~feeding_style) +
  geom_smooth(aes(mg, log10(counts+1)), method = "lm")


summary(lm(log10(counts+1) ~ do_top2m,
           data = macro_mobility_full %>%
             pivot_longer(cols = c(fa, sa, s), names_to = "mobility", values_to = "counts") %>%
             filter(mobility == "s")))

macro_feeding %>%
  select(park_code, site_code, event_year, 
         filter_feeder, unlabeled, predator, gathering_collector, shredder_herbivore) %>%
  pivot_longer(cols = c(filter_feeder, unlabeled, predator, gathering_collector, shredder_herbivore), 
               names_to = "feeding_style", values_to = "counts") %>%
  group_by(park_code, site_code, event_year) %>%
  mutate(total_invert = sum(counts),
         prop_invert = counts/total_invert) %>%
ggplot(aes(x = event_year, y = prop_invert, fill = feeding_style)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~site_code) +
  theme_bw()
