library(tidyverse)
library(readxl)
library(visdat)
library(vegan)
library(janitor)

zoop <- read_excel("../data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")
# There's a row where "Collotheca pelagica" is pasted across three columns
# It is treated as an NA and Taxonid is retained as numeric
# Admittedly I don't have a full grasp of the last three cols, but I'll make
# the record match with the others of that species nearby:
zoop[1487, "Taxa"] <- "ROT"
zoop[1487, "Taxonid"] <- 5

# Quick view of the whole dataset:
vis_dat(zoop) # data types of the data frame
vis_miss(zoop) # missing data

# Three additional records where Taxa and Taxonid aren't complete.
# They make sense
zoop %>% filter(is.na(Taxa) | is.na(Taxonid))

# Genus-species by year:
zoop_lake_year_gensp <- zoop %>%
  filter(!is.na(Taxa),
         !is.na(Taxonid)) %>%
  ggplot() +
  geom_bar(aes(x = Year, y = Density, fill = GenSp),
           stat = "identity", position = "fill") +
  #geom_point(aes(x = Year, y = Density, color = Taxa)) +
  facet_wrap(. ~ Lake) +
  theme(axis.text.x = element_text(angle = 69, hjust = 1),
        legend.position = "bottom") +
  ggtitle("Relative densities of zooplankton by lake and year")

ggsave(filename = "../figures/paneled_by_lake/zoop_year_dens_gensp.png",
       plot = zoop_lake_year_gensp, device = "png", width = 12, height = 12,
       units = "in")

# Taxa by year:
zoop_lake_year_taxa <- zoop %>%
  filter(!is.na(Taxa),
         !is.na(Taxonid)) %>%
  ggplot() +
  geom_bar(aes(x = Year, y = Density, fill = Taxa),
           stat = "identity", position = "fill") +
  #geom_point(aes(x = Year, y = Density, color = Taxa)) +
  facet_wrap(. ~ Lake) +
  theme(axis.text.x = element_text(angle = 69, hjust = 1),
        legend.position = "right") +
  ggtitle("Relative densities of zooplankton by lake and year")

ggsave(filename = "../figures/paneled_by_lake/zoop_year_dens_taxa.png",
       plot = zoop_lake_year_taxa, device = "png", width = 12, height = 12,
       units = "in")


# Multivariate attempt ----------------------------------------------------

# Not looking good for this being of any use
zoop_mat <- zoop %>%
  mutate(park_year_lake = paste(Park, Year, Lake),
         i = row_number()) %>%
  spread(key = GenSp, value = Density, fill = 0) %>%
  clean_names() %>%
  filter(year == 2011) %>%
  select(alona_costata:trichotria_tetractis) %>%
  as.matrix()




zoop_nmds <- metaMDS(zoop_mat,
                     k = 2,
                     try = 20)

zoop_pca <- prcomp(vegdist(zoop_mat, method = "bray",
                          diag = TRUE, upper = FALSE),
                  scale. = T,
                  center = T)

plot(zoop_pca, type = "l", main = "")
biplot(zoop_pca) # plot PCA in base R

pca.results <- data.frame(zoop_pca$x)
nla.pca.plot <- ggplot(data = pca.results,
                       aes(x = PC1, y = PC2)) +
  geom_point() +
  theme(text = element_text(size = 19))

nla.pca.plot

