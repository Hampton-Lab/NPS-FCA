library(tidyverse)
library(readxl)
library(vegan)
library(labdsv)
library(vegan3d)

# Load data
macro_counts <- read_excel( "~/data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "BMI_Counts")

#macro_lookup <- read_excel(path = "../data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
#                           sheet = "Taxon_Lookup")
#using a sheet MM and SH modified to include feeding guild - MB will want to clean this
#up, SH just doing a quick and dirty modification
macro_lookup<-read_excel("NPS_macroinvert_lookup_20210403.xlsx")

head(macro_counts)

head(macro_lookup)


# Want to look at whether the counted macros taxa match the taxa list provided
unique_counts <- unique(macro_counts$Taxon)
unique_lookup <- unique(macro_lookup$Taxon)

# Which taxa in the counts data aren't in the lookup data? (result = 4)
unique_counts[!(unique_counts %in% unique_lookup)]

# Which taxa in lookup aren't in counts? (result = 0)
unique_lookup[!(unique_lookup %in% unique_counts)]


# The lists are pretty well matched. SH suggested taking a look at family-level
# aggregation for multivariate approach
full_macro <- inner_join(x = macro_counts, y = macro_lookup, by = c("Taxon"))
unique(full_macro$Family)

head(full_macro)
unique(full_macro$Feeding_Guild)

# Count column has an NA value...unclear why. I remove it. 
summary(full_macro)
full_macro <- full_macro %>%
  filter(!is.na(Count))

# The family grouping with the second highest number of data points is 'NA'
full_macro %>%
  count(Family) %>%
  arrange(n) %>%
  print(n = nrow(.))

# Not sure how much of an issue this is?
# How balanced is it across lakes?
full_macro %>%
  filter(is.na(Family)) %>%
  count(Park, Site_ID)

# NOCA has 256, MORA 206

full_macro %>%
  filter(is.na(Family)) %>%
  count(Order) %>%
  print(n = nrow(.))
# Not clear on what the "_" prefixed Orders are. Placeholders?

# Note that I haven't filtered out for specific life stage(s)

full_macro %>%
  filter(is.na(Feeding_Guild)) %>%
  count(Park, Site_ID) %>%
  print(n = nrow(.))

# Make community data matrix
# macro_comm <- full_macro %>%
#  group_by(Park, Site_ID, Year, Order) %>%
#  summarise(sum_taxa = sum(Count)) %>%
#  mutate(park_site = paste0(Park, "_", Site_ID, "_", Year)) %>%
#  ungroup() %>%
 # select(park_site, Order, sum_taxa) %>%
# data.frame() %>%
#  matrify()

#make community data matrix by feeding guild
macro_comm <- full_macro %>%
  group_by(Park, Site_ID, Year, Feeding_Guild) %>%
  summarise(sum_taxa = sum(Count)) %>%
  mutate(park_site = paste0(Park, "_", Site_ID, "_", Year)) %>%
  ungroup() %>%
  select(park_site, Feeding_Guild, sum_taxa) %>%
  data.frame() %>%
  matrify()
#nmds plot
nmds_FG <- metaMDS(comm = macro_comm, distance = "bray", k = 2)
plot(nmds_FG)
stressplot(nmds_FG)

#2-d plots of feeding guild mds
nmds_points <- data.frame(nmds_FG$points) %>%
  bind_cols(park_site_year = row.names(.), .) %>%
  separate(data = ., col = park_site_year, into = c("park", "site", "year"),
           sep = "_") %>%
  arrange(park, site, year)

ggplot(data = nmds_points) +
  geom_point(aes(x = MDS1, y = MDS2, color = park)) 


#3-d plots of feeding guilds
nmds_FG3 <- metaMDS(comm = macro_comm, distance = "bray", k = 3)
plot(nmds_FG3)
stressplot(nmds_FG3)

ordiplot(ord = nmds_FG3, type = "text", display = "species")
ordiplot3d(nmds_FG3)

pl <- ordiplot3d(nmds_FG3, scaling = "symmetric", angle=15, type="n")
points(pl, "points", pch=16, col="red", cex = 0.7)

sp <- scores(nmds_FG3, choices=1:3, display="species", scaling="symmetric")
text(pl$xyz.convert(sp), rownames(sp), cex=0.7, xpd=TRUE)

#2-d plots of feeding guild mds
nmds_points <- data.frame(nmds_FG3$points) %>%
  bind_cols(park_site_year = row.names(.), .) %>%
  separate(data = ., col = park_site_year, into = c("park", "site", "year"),
           sep = "_") %>%
  arrange(park, site, year)

ggplot(data = nmds_points) +
  geom_point(aes(x = MDS1, y = MDS2, color = park)) 

ggplot(data = nmds_points) +
  geom_point(aes(x = MDS2, y = MDS3, color = park)) 
