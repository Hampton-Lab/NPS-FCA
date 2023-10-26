
data_path <- "./output"


CLAD_evalution_all <- list.files(data_path, pattern = "global_random_forest_CLAD")%>%
  map(~ readRDS(file.path(data_path, .))) %>%
  data.table::rbindlist(fill = T)%>%
  group_by(site_code)%>%
  summarize(NSE = NSE(obs_density, predicted_density),
            RMSE = RMSE(obs_density, predicted_density))

table <- CLAD_evalution_all
pdf("./figures/Evalution_table_CLAD.pdf", height=24, width=6)
grid.table(table)
dev.off()


COPE_evalution_all <- list.files(data_path, pattern = "global_random_forest_COPE")%>%
  map(~ readRDS(file.path(data_path, .))) %>%
  data.table::rbindlist(fill = T)%>%
  group_by(site_code)%>%
  summarize(NSE = NSE(obs_density, predicted_density),
            RMSE = RMSE(obs_density, predicted_density))

table <- COPE_evalution_all
pdf("./figures/Evalution_table_COPE.pdf", height=24, width=6)
grid.table(table)
dev.off()

MICRO_evalution_all <- list.files(data_path, pattern = "global_random_forest_MICRO")%>%
  map(~ readRDS(file.path(data_path, .))) %>%
  data.table::rbindlist(fill = T)%>%
  group_by(site_code)%>%
  summarize(NSE = NSE(obs_density, predicted_density),
            RMSE = RMSE(obs_density, predicted_density))

table <- MICRO_evalution_all
pdf("./figures/Evalution_table_MICRO.pdf", height=24, width=6)
grid.table(table)
dev.off()
