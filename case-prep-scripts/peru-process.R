# load district-level data
df<-readRDS("PER_adm3_cases.rds") %>%
      group_by(departamento, year, week) %>% 
      summarize(cases = sum(cases)) %>%
      rename(id = departamento)

map <- read_sf("maps/per_admbnda_adm1_ign_20200714.shp") %>%
  st_drop_geometry() %>%
  select(ADM1_PCODE, ADM1_ES) %>% 
  mutate(ADM1_ES = toupper(ADM1_ES))

# add adm1_pcode
df %<>% left_join(map, by = c("id" = "ADM1_ES")) 

write.csv(df, "cases/PER_cases.csv", row.names=FALSE)
