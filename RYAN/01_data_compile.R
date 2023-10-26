#########################
# ZOOPLANKTON COMPILATION
#########################

df <- read_excel("./data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")
df[1487, "Taxa"] <- "ROT"
df[1487, "Taxonid"] <- 5

zoop_no_rotifers <- df %>%
  filter(Taxa != "ROT")%>%
  filter(Taxa != "INS")

#Partition Rotifers into functional feeding groups
rot_wFFG <- df %>% filter(Taxa == "ROT")
rot_wFFG$GenSp <- sub(" .*", "", rot_wFFG$GenSp)

rot_wFFG <- rot_wFFG %>%
  mutate(trophi = case_when(
    GenSp == "Conochilius" ~ "malleoramate",
    GenSp == "Kellicottia" ~ "malleate",
    GenSp == "Keratella" ~ "malleate",
    GenSp == "Lecane" ~ "malleate",
    GenSp == "Monostyla" ~ "malleate",
    GenSp == "Polyarthra" ~ "virgate",
    GenSp == "Trichotria" ~ "malleate",
    GenSp == "Synchaeta" ~ "virgate",
    GenSp == "Collotheca" ~ "uncinate",
    GenSp == "Notholca" ~ "malleate",
    GenSp == "Brachionus" ~ "malleate",
    GenSp == "Gastropus" ~ "virgate",
    GenSp == "Philodina" ~ "malleate",
    GenSp == "Ascomorpha" ~ "virgate",
    GenSp == "Filinia" ~ "malleoramate",
    GenSp == "Trichocerca" ~ "virgate",
    GenSp == "Ploesoma" ~ "virgate",
    GenSp == "Conochiloides" ~ "malleoramate",
    TRUE ~ NA_character_),
    feed_type = case_when(
      trophi == "uncinate" ~ "RAP",
      trophi == "virgate" ~ "RAP",
      trophi == "malleate" ~ "MICRO",
      trophi == "malleoramate" ~ "MICRO",
      TRUE ~ NA_character_))%>%
  select(`Park&Year&Lake`,Park, Year, Lake, Code, Density, GenSp, feed_type, Taxonid)%>%
  rename(Taxa = feed_type)

match_sites <- readRDS(file = "./data/name_site_matches.rds")

zoop <- bind_rows(zoop_no_rotifers, rot_wFFG)%>%
  left_join(x = ., y = match_sites, by = c("Lake" = "old_name"))%>%
  clean_names()%>%
  group_by(park, site_code, lake, year, taxa) %>%
  summarise(density = sum(density))%>%
  ungroup() %>%
  mutate(park_site_year = paste0(park, "_", lake, "_", year)) %>%
  select(park_site_year, taxa, density) %>%
  data.frame() %>%
  matrify()%>%
  select(-HAR)%>%
  rownames_to_column(.) %>%
  separate(col = rowname, into = c("park_code", "site_code", "event_year"), sep = "_")%>%
  mutate(event_year = as.character(event_year))%>%
  mutate(RAP = log10(RAP+1),
         MICRO = log10(MICRO+1),
         CLAD = log10(CLAD+1),
         COPE = log10(COPE+1))

########################################
# PHYSICAL AND CHEMICAL DATA COMPILATION
########################################

env_dat_all <- readRDS(file = "./data/bigjoin.rds")
env_dat_yr <- env_dat_all %>%
  filter(variable %in% c("Ca", "Chlorophyll","TDS","DOC", "SpCond_top2m", "Total N","Total P",
                         "BotTemp","MidTemp","SurfTemp", "ice_out_doy","SWE_May_snotel","DO_below2m",
                         "BRK_ever","CCT_ever","RBT_ever","TSS_ever","WCT_ever","secchi_value_m")) %>%
  unique() %>%
  select(park_code, Lake, site_code, event_year, variable, value, solar_jas, Elevation_m,flush_index_SWE_May_snotel, Depth_max,SWE_May_snotel, lon, lat)%>%
  spread(key = variable, value = value)%>%
  mutate(event_year = as.character(event_year))%>%
  select(-site_code)%>%
  rename(site_code = Lake)

env_dat_yr$site_code <- gsub("Lake ", "", env_dat_yr$site_code)
env_dat_yr$site_code <- gsub(" Lake", "", env_dat_yr$site_code)
env_dat_yr$site_code <- gsub("Upper ", "", env_dat_yr$site_code)
env_dat_yr$site_code <- gsub("Lower ", "", env_dat_yr$site_code)
env_dat_yr$site_code <- gsub("Easy Ridge", "EasyRidge", env_dat_yr$site_code)
env_dat_yr$site_code <- gsub("Lower ", "", env_dat_yr$site_code)
env_dat_yr$site_code <- gsub("La Crosse", "LaCrosse", env_dat_yr$site_code)

env_zoop_data <- env_dat_yr %>% left_join(., zoop, by = c("park_code","site_code","event_year")) %>%
  filter(site_code != "La Crosse" & site_code != "Palisades" & site_code != "Hoh")%>%
  group_by(site_code, park_code)%>%
  mutate(fish = ifelse((CCT_ever+TSS_ever+WCT_ever+RBT_ever+BRK_ever)>0,1,0))%>%
  select(-CCT_ever, -TSS_ever, -WCT_ever, -RBT_ever, -BRK_ever)%>%
  mutate(lake_temp = (SurfTemp+BotTemp+MidTemp)/3,
         stability = SurfTemp - BotTemp,
         delta_temp = lake_temp - lag(lake_temp),
         delta_temp = ifelse(is.nan(delta_temp),NA,delta_temp))%>%
  select(-SurfTemp,-BotTemp,-MidTemp)%>%
  mutate_at(vars(-site_code, -park_code, -event_year),
            funs(imputeTS::na_interpolation(., option = "spline")))%>%
  mutate_at(vars(Ca, Chlorophyll, DOC, TDS, SpCond_top2m,`Total N`, ice_out_doy,lake_temp,
                 `Total P`,stability),
            funs((log10(.+1))))%>%
  mutate_at(vars(-site_code, -park_code,-Elevation_m,-solar_jas, -event_year, -delta_temp),
            funs(imputeTS::na_interpolation(., option = "spline")))%>%
  ungroup(.)%>%
  filter(RAP >= 0)%>%
  filter(stability>0)

ZOOP_all <- env_zoop_data %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll, fish)%>%
  mutate(site_code = case_when(
    site_code == "LH15" ~ 1,
    site_code == "Allen" ~ 2,
    site_code == "LP19" ~ 3,
    site_code == "Deadwood" ~ 4,
    site_code == "Blue" ~ 5,
    site_code == "Blum" ~ 6,
    site_code == "Silent" ~ 7,
    site_code == "EasyRidge" ~ 8,
    site_code == "East" ~ 9,
    site_code == "Bowan" ~ 10,
    site_code == "Triplet" ~ 11,
    site_code == "Gladys" ~ 12,
    site_code == "Ferry" ~ 13,
    site_code == "Heather" ~ 14,
    site_code == "Crazy" ~ 15,
    site_code == "Milk" ~ 16,
    site_code == "LaCrosse" ~ 17,
    site_code == "Sunup" ~ 18,
    site_code == "Connie" ~ 19)) %>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll","fish"))


alpha_by_year <- ZOOP_all %>% filter(event_year == 2011) %>%
  select(site_code, COPE, CLAD, RAP, MICRO) %>% arrange(site_code) %>%
  remove_rownames %>% column_to_rownames(var="site_code")

alpha.div(alpha_by_year,"simp")
SW.index(alpha_by_year)

group_by(site_code,event_year,park_code) %>%
  alpha.div(.,"simp")


gamma_diversity_ZOOP <- ZOOP_all %>%
  group_by(variable, event_year, site_code, park_code) %>%
  mutate(value = ifelse(value<=0,0,value)) %>%
  summarise(value = sum(value))%>%
  group_by(site_code, event_year) %>%
  summarize(gamma = vegan::diversity(value))

ggplot(alpha_diversity_ZOOP)+
  geom_point(aes(event_year, simpson, color = park_code))

beta_diversity_ZOOP <- ZOOP_all %>%
  group_by(variable, event_year, park_code) %>%
  mutate(value = ifelse(value<=0,0,value)) %>%
  summarise(value = sum(value))%>%
  group_by(park_code, event_year) %>%
  summarize(simpson = vegan::diversity(value,
                                     index = "simpson"))

gamma_diversity_ZOOP <- ZOOP_all %>%
  group_by(variable, event_year) %>%
  mutate(value = ifelse(value<=0,0,value)) %>%
  summarise(value = sum(value))%>%
  group_by(event_year) %>%
  summarize(alpha = vegan::diversity(value,
                                     index = "simpson"))


ZOOP_fishless <- env_zoop_data %>% filter(fish == 0) %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll)%>%
  mutate(site_code = case_when(
    site_code == "Allen" ~ 1,
    site_code == "Bowan" ~ 2,
    site_code == "Milk" ~ 3,
    site_code == "Silent" ~ 4,
    site_code == "Triplet" ~ 5,
    site_code == "Sunup" ~ 6,
    site_code == "Blum" ~ 7,
    site_code == "Connie" ~ 8,
    site_code == "Crazy" ~ 9,
    site_code == "East" ~ 10,
    site_code == "EasyRidge" ~ 11,
    site_code == "Ferry" ~ 12,
    site_code == "LaCrosse" ~ 13))%>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll"))


ZOOP_fish <- env_zoop_data %>% filter(fish == 1) %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll)%>%
  mutate(site_code = case_when(
    site_code == "Blue" ~ 1,
    site_code == "Gladys" ~ 2,
    site_code == "Heather" ~ 3,
    site_code == "LP19" ~ 4,
    site_code == "Deadwood" ~ 5,
    site_code == "LH15" ~ 6))%>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll"))

ZOOP_by_site <- env_zoop_data %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll, fish)%>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll","fish"))

###############################
# MACROINVERTEBRATE COMPILATION
###############################

# Macroinvert data
macro_counts <- read_excel(path = "./data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "BMI_Counts")
# Lookup table for macro taxonomy
macro_lookup <- read_excel(path = "./data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "Taxon_Lookup")

full_macro <- inner_join(x = macro_counts, y = macro_lookup, by = c("Taxon"))%>%
  filter(!is.na(Count)) %>%
  group_by(Park, Site_ID, Year, Genus) %>%
  summarise(sum_phylum_count = sum(Count))%>%
  rename(park_code = Park,
         site_code = Site_ID,
         event_year = Year)%>%
  melt(., id.vars = c("park_code","site_code","event_year","Genus"))%>%
  mutate(across(everything(), gsub, pattern = "_", replacement = ""))%>%
  mutate(event_year = as.character(event_year),
         value = as.numeric(value))%>%
  spread(., key = Genus, value = value)

full_macro[is.na(full_macro)] <- 0

macro_join <- env_dat_yr %>% left_join(., full_macro, by = c("park_code","site_code","event_year"))%>%
  select(-variable)%>%
  filter(site_code != "Palisades" & site_code != "EasyRidge")%>%
  filter(park_code !=  "OLYM")%>%
  group_by(site_code, park_code)%>%
  mutate(fish = ifelse((CCT_ever+TSS_ever+WCT_ever+RBT_ever+BRK_ever)>0,1,0))%>%
  select(-CCT_ever, -TSS_ever, -WCT_ever, -RBT_ever, -BRK_ever)%>%
  mutate(lake_temp = (SurfTemp+BotTemp+MidTemp)/3,
         stability = SurfTemp - BotTemp,
         delta_temp = lake_temp - lag(lake_temp))%>%
  select(-SurfTemp,-BotTemp,-MidTemp)%>%
  mutate_at(vars(-site_code, -park_code, -Elevation_m,-solar_jas, -event_year, -lon, -lat, -DOC, -fish, -delta_temp, -SWE_May_snotel),
            funs((log10(.+1))))%>%
  mutate(DOC = ifelse(DOC<0,0,DOC))%>%
  mutate(DOC = log10(DOC+1))%>%
  ungroup(.) %>%
  select(event_year, site_code, park_code, Ablabesmyia,
         Aeshna,Agabini,Agabus,Alotanypus,Ameletus,
         Apatania,Atrichopogon,Baetis,`Bezzia/Palpomyia`,Boreochlus,
         Brillia,Caecidotea,Callibaetis,Capniidae,Ceratopogon,
         Chaetocladius,Chaoborus,Chironomus,Chrysops,Cinygma,
         Cinygmula,Cladopelma,Cladotanytarsus,Clinocera,Clistoronia,
         Cordulia,Corynoneura,Crangonyx,Cricotopus,Cryptochironomus,
         Culicoides,Dasyhelea,Desmona,Diamesa,Dicosmoecus,
         Dicranota,Dicrotendipes,Diplocladius,Dixa,Dixella,
         Drunella,Ecclisocosmoecus,Ecclisomyia,Endochironomus,Epeorus,
         Ephemerella,Ephydridae,Eukiefferiella,Ferrissia,Forcipomyia,
         Gammarus,Glyptotendipes,Halesochila,Heleniella,Helobdella,
         Hesperoconopa,Hesperophylax,Heterlimnius,Heterotrissocladius,Hexatoma,
         Hyalella,Hydra,Hydrobaenus,Isoperla,Larsia,
         Leuctridae,Limnephilus,Limnophila,Limnophyes,Macropelopia,
         Megarcys,Metriocnemus,Micropsectra,Microtendipes,Molophilus,
         Monodiamesa,`Moselia infuscata`,Musculium,Mystacides,Nanocladius,
         Neoleptophlebia,Neothremma,Notonecta,Odontomesa,Oreogeton,
         Orthocladius,Pagastia,Pagastiella,Parachaetocladius,Parachirnomus,
         Paracladopelma,Paraleptophlebia,Paraleuctra,Paramerina,Parametriocnemus,
         Paraphaenocladius,Paratanytarsus,Parochlus,Parorthocladius,Phaenopsectra,
         Philocasca,Phryganeidae,Pisidium,Platysmittia,Polycentropus,
         Polypedilum,Potthastia,Procladius,Prodiamesa,Protanypus,
         Psectrocladius,Psectrotanypus,Pseudodiamesa,Pseudorthocladius,Pseudosmittia,
         Psilometriocnemus,Psychoda,Psychoglypha,Ranatra,Rheocricotopus,
         Rhyacophila,Sanfilippodytes,Sergentia,Setvena,Sialis,
         Simulium,Siphlonurus,Somatochlora,Sphaerium,Stempellinella,
         Stictochironomus,Stictotarsus,Stratiomys,Sublettea,Suwallia,
         Sweltsa,Synorthocladius,Tanytarsus,Thermonectus,Thienemanniella,
         Thienemannimyia,Tipula,Tvetenia,`Visoka cataractae`,Wiedemannia,
         Yoraperla,`Zapada cinctipes`,`Zapada columbiana`,`Zapada frigida`,`Zapada oregonensis group`,
         Zavrelimyia, Ca, lake_temp, Chlorophyll, fish)%>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll","fish")) %>%
  mutate(FFG = case_when(
    variable == "Ablabesmyia" ~ "GCR",
    variable == "Aeshna" ~ "PRED",
    variable == "Agabini" ~ "PRED",
    variable == "Agabus" ~ "PRED",
    variable == "Alotanypus" ~ "GCR",
    variable == "Ameletus" ~ "GCR",
    variable == "Apatania" ~ "GCR",
    variable == "Atrichopogon" ~ "GCR",
    variable == "Baetis" ~ "GCR",
    variable == "Bezzia/Palpomyia" ~ "PRED",
    variable == "Boreochlus" ~ "GCR",
    variable == "Brillia" ~ "GCR",
    variable == "Caecidotea" ~ "GCR",
    variable == "Callibaetis" ~ "GCR",
    variable == "Capniidae" ~ "SHR",
    variable == "Ceratopogon" ~ "PRED",
    variable == "Chaetocladius" ~ "GCR",
    variable == "Chaoborus" ~ "PRED",
    variable == "Chironomus" ~ "GCR",
    variable == "Chrysops" ~ "PRED",
    variable == "Cinygma" ~ "SCR",
    variable == "Cinygmula" ~ "SCR",
    variable == "Cladopelma" ~ "GCR",
    variable == "Cladotanytarsus" ~ "FCR",
    variable == "Clinocera" ~ "PRED",
    variable == "Clistoronia" ~ "SHR",
    variable == "Cordulia" ~ "PRED",
    variable == "Corynoneura" ~ "GCR",
    variable == "Crangonyx" ~ "GCR",
    variable == "Cricotopus" ~ "GCR",
    variable == "Cryptochironomus" ~ "GCR",
    variable == "Culicoides" ~ "GCR",
    variable == "Dasyhelea" ~ "GCR",
    variable == "Desmona" ~ "SHR",
    variable == "Diamesa" ~ "GCR",
    variable == "Dicosmoecus" ~ "SHR",
    variable == "Dicranota" ~ "SHR",
    variable == "Dicrotendipes" ~ "GCR",
    variable == "Diplocladius" ~ "GCR",
    variable == "Dixa" ~ "FCR",
    variable == "Dixella" ~ "FCR",
    variable == "Drunella" ~ "GCR",
    variable == "Ecclisocosmoecus" ~ "SHR",
    variable == "Ecclisomyia" ~ "SHR",
    variable == "Endochironomus" ~ "GCR",
    variable == "Epeorus" ~ "SCR",
    variable == "Ephemerella" ~ "GCR",
    variable == "Ephydridae" ~ "PRED",
    variable == "Eukiefferiella" ~ "GCR",
    variable == "Ferrissia" ~ "SCR",
    variable == "Forcipomyia" ~ "GCR",
    variable == "Gammarus" ~ "GCR",
    variable == "Glyptotendipes" ~ "GCR",
    variable == "Halesochila" ~ "SHR",
    variable == "Heleniella" ~ "GCR",
    variable == "Helobdella" ~ "PRED",
    variable == "Hesperoconopa" ~ "SHR",
    variable == "Hesperophylax" ~ "SHR",
    variable == "Heterlimnius" ~ "SHR",
    variable == "Heterotrissocladius" ~ "GCR",
    variable == "Hexatoma" ~ "SHR",
    variable == "Hyalella" ~ "GCR",
    variable == "Hydra" ~ "PRED",
    variable == "Hydrobaenus" ~ "GCR",
    variable == "Isoperla" ~ "PRED",
    variable == "Larsia" ~ "GCR",
    variable == "Leuctridae" ~ "SHR",
    variable == "Limnephilus" ~ "SHR",
    variable == "Limnophila" ~ "SHR",
    variable == "Limnophyes" ~ "GCR",
    variable == "Macropelopia" ~ "GCR",
    variable == "Megarcys" ~ "PRED",
    variable == "Metriocnemus" ~ "GCR",
    variable == "Micropsectra" ~ "GCR",
    variable == "Microtendipes" ~ "GCR",
    variable == "Molophilus" ~ "SHR",
    variable == "Monodiamesa" ~ "GCR",
    variable == "Moselia infuscata" ~ "SHR",
    variable == "Musculium" ~ "FCR",
    variable == "Mystacides" ~ "GCR",
    variable == "Nanocladius" ~ "GCR",
    variable == "Neoleptophlebia" ~ "GCR",
    variable == "Neothremma" ~ "SCR",
    variable == "Notonecta" ~ "PRED",
    variable == "Odontomesa" ~ "GCR",
    variable == "Oreogeton" ~ "PRED",
    variable == "Orthocladius" ~ "GCR",
    variable == "Pagastia" ~ "GCR",
    variable == "Pagastiella" ~ "GCR",
    variable == "Parachaetocladius" ~ "GCR",
    variable == "Parachirnomus" ~ "GCR",
    variable == "Paracladopelma" ~ "GCR",
    variable == "Paraleptophlebia" ~ "GCR",
    variable == "Paraleuctra" ~ "SHR",
    variable == "Paramerina" ~ "GCR",
    variable == "Parametriocnemus" ~ "GCR",
    variable == "Paraphaenocladius" ~ "GCR",
    variable == "Paratanytarsus" ~ "GCR",
    variable == "Parochlus" ~ "GCR",
    variable == "Parorthocladius" ~ "GCR",
    variable == "Phaenopsectra" ~ "GCR",
    variable == "Philocasca" ~ "SHR",
    variable == "Phryganeidae" ~ "SHR",
    variable == "Pisidium" ~ "FCR",
    variable == "Platysmittia" ~ "GCR",
    variable == "Polycentropus" ~ "PRED",
    variable == "Polypedilum" ~ "GCR",
    variable == "Potthastia" ~ "GCR",
    variable == "Procladius" ~ "GCR",
    variable == "Prodiamesa" ~ "GCR",
    variable == "Protanypus" ~ "GCR",
    variable == "Psectrocladius" ~ "GCR",
    variable == "Psectrotanypus" ~ "GCR",
    variable == "Pseudodiamesa" ~ "GCR",
    variable == "Pseudorthocladius" ~ "GCR",
    variable == "Pseudosmittia" ~ "GCR",
    variable == "Psilometriocnemus" ~ "GCR",
    variable == "Psychoda" ~ "GCR",
    variable == "Psychoglypha" ~ "SHR",
    variable == "Ranatra" ~ "PRED",
    variable == "Rheocricotopus" ~ "GCR",
    variable == "Rhyacophila" ~ "PRED",
    variable == "Sanfilippodytes" ~ "PRED",
    variable == "Sergentia" ~ "GCR",
    variable == "Setvena" ~ "PRED",
    variable == "Sialis" ~ "PRED",
    variable == "Simulium" ~ "FCR",
    variable == "Siphlonurus" ~ "GCR",
    variable == "Somatochlora" ~ "PRED",
    variable == "Sphaerium" ~ "FCR",
    variable == "Stempellinella" ~ "GCR",
    variable == "Stictochironomus" ~ "GCR",
    variable == "Stictotarsus" ~ "PRED",
    variable == "Stratiomys" ~ "GCR",
    variable == "Sublettea" ~ "GCR",
    variable == "Suwallia" ~ "SHR",
    variable == "Sweltsa" ~ "SHR",
    variable == "Synorthocladius" ~ "GCR",
    variable == "Tanytarsus" ~ "GCR",
    variable == "Thermonectus" ~ "PRED",
    variable == "Thienemanniella" ~ "GCR",
    variable == "Thienemannimyia" ~ "GCR",
    variable == "Tipula" ~ "SHR",
    variable == "Tvetenia" ~ "GCR",
    variable == "Visoka cataractae" ~ "SHR",
    variable == "Wiedemannia" ~ "PRED",
    variable == "Yoraperla" ~ "PRED",
    variable == "Zapada cinctipes" ~ "SHR",
    variable == "Zapada columbiana" ~ "SHR",
    variable == "Zapada frigida" ~ "SHR",
    variable == "Zapada oregonensis group" ~ "SHR",
    variable == "Zavrelimyia" ~ "GCR",
    TRUE ~ NA_character_))%>%
  group_by(site_code)%>%
  mutate_at(vars(-site_code, -park_code, -event_year, -variable, -FFG),
          funs(imputeTS::na_interpolation(., option = "linear")))

macro_aggregate <- macro_join %>%
  group_by(site_code, variable, FFG, park_code, event_year) %>%
  summarize(sum_species = sum(value),
            lake_temp = mean(lake_temp),
            Ca = mean(Ca),
            Chlorophyll = mean(Chlorophyll),
            fish = mean(fish)) %>%
  mutate(sum_species = as.numeric(sum_species))%>%
  spread(.,                                  # Applying spread function
         key = FFG,
         value = sum_species) %>%
  mutate_at(vars(-site_code, -event_year),
            funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  ungroup(.)

macro_aggregate %>%
  group_by(variable, event_year, site_code) %>%
  mutate(sum_species = ifelse(sum_species<=0,0,sum_species)) %>%
  summarise(sum_species = sum(sum_species))%>%
  group_by(site_code, event_year) %>%
  summarize(alpha = vegan::diversity(sum_species,
                                     index = "simpson")) %>%
  ggplot(., aes(event_year, alpha, color = site_code))+
  geom_point()

macro_aggregate %>%
  group_by(variable, event_year, park_code) %>%
  mutate(sum_species = ifelse(sum_species<=0,0,sum_species)) %>%
  summarise(sum_species = sum(sum_species))%>%
  group_by(park_code, event_year) %>%
  summarize(alpha = vegan::diversity(sum_species,
                                     index = "simpson"))%>%
  ggplot(., aes(event_year, alpha, color = park_code))+
  geom_point()

gamma_diversity_BMI <- macro_aggregate %>%
  group_by(variable, event_year) %>%
  mutate(sum_species = ifelse(sum_species<=0,0,sum_species)) %>%
  summarise(sum_species = sum(sum_species))%>%
  group_by(event_year) %>%
  summarize(gamma = vegan::diversity(sum_species,
                                     index = "simpson"))%>%
  ggplot(., aes(event_year, gamma))+geom_point()

