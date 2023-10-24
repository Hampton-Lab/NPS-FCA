library(tidyverse)
library(readxl)
library(vegan)
library(labdsv)

# Load data
macro_counts <- read_excel(path = "../data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "BMI_Counts")

macro_lookup <- read_excel(path = "../data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "Taxon_Lookup")

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
  #filter(is.na(Family)) %>%
  count(Order) %>%
  print(n = nrow(.))
# Not clear on what the "_" prefixed Orders are. Placeholders?

# Note that I haven't filtered out for specific life stage(s)

# Make community data matrix
macro_comm <- full_macro %>%
  group_by(Park, Site_ID, Year, Order) %>%
  summarise(sum_taxa = sum(Count)) %>%
  mutate(park_site = paste0(Park, "_", Site_ID, "_", Year)) %>%
  ungroup() %>%
  select(park_site, Order, sum_taxa) %>%
  data.frame() %>%
  matrify()

# Standardize to relative abundance per site
macro_std <- decostand(x = macro_comm, method = "total")

# Check
sum(macro_std[1, ])


# From https://ourcodingclub.github.io/2018/05/04/ordination.html
scree_plot <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10),
       replicate(10, metaMDS(comm = x, distance = "bray", k = 1)$stress),
       xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions",
       ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x,  distance = "bray", k = i + 1)$stress))
  }
}

# Plot stress by dimensions
scree_plot(macro_std)

# Two dimensions doesn't converge
nmds_2 <- metaMDS(comm = macro_std, distance = "bray", k = 2)
# Three dimensions converges:
nmds_3 <- metaMDS(comm = macro_std, distance = "bray", k = 3)
stressplot(nmds_3)

ordiplot(ord = nmds_3, type = "text", display = "species")

nmds_points <- data.frame(nmds_3$points) %>%
  bind_cols(park_site_year = row.names(.), .) %>%
  separate(data = ., col = park_site_year, into = c("park", "site", "year"),
           sep = "_") %>%
  arrange(park, site, year)

# Points only
ggplot(data = nmds_points) +
  geom_point(aes(x = MDS1, y = MDS2, color = site)) +
  geom_text(aes(x = MDS1, y = MDS2, color = site, label = year)) 


# Points connected by line
ggplot(data = nmds_points) +
  geom_point(aes(x = MDS1, y = MDS2, color = site)) +
  geom_path(aes(x = MDS1, y = MDS2, color = site)) +
  geom_text(aes(x = MDS1, y = MDS2, color = site, label = year)) 


# It's a bit cluttered. Perhaps try going back and removing contribs < 5%,
# then re-run? First take a look at parks:

# Points only
ggplot(data = nmds_points) +
  geom_point(aes(x = MDS1, y = MDS2, color = park)) +
  geom_text(aes(x = MDS1, y = MDS2, color = park, label = paste(year, " ", site)))





