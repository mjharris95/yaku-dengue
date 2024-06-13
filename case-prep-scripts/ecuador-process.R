library(readxl)
library(tidyverse)
library(magrittr)
library(sf)

df <- read_excel("ecuador-cases-raw.xlsx", sheet="2014-2023") %>%
          rename("year" = 2,
                 "week" = "SE",
                 "cases" = "CASOS")

# sum different types of dengue and different reports
df %<>% group_by(Canton, Provincia, week, year)%>%
        summarize(cases = sum(cases)) %>%
        rename(id = "Canton")

# some of the special character got messed up, manually correct
df %<>%
  mutate(id = ifelse(id == "CRNEL. MARCELINO MARIDUE?A", "CRNEL. MARCELINO MARIDUEÑA", id)) %>%
  mutate(id = ifelse(id == "LOGRO?O", "LOGROÐO", id)) %>%
  mutate(id = ifelse(id == "LOGROÑO", "LOGROÐO", id)) %>%
  mutate(id = ifelse(id == "O?A", "OÑA", id)) %>%
  mutate(id = ifelse(id == "RUMIÑAHUI", "RUMIÐAHUI", id)) %>% 
  mutate(id = ifelse(id == "RUMI?AHUI", "RUMIÐAHUI", id)) %>%
  mutate(id = ifelse(id == "CA?AR", "CAÑAR", id)) %>%
  mutate(id = ifelse(id == "EL EMPALME", "EMPALME", id)) %>%
  mutate(id = ifelse(id == "PI?AS", "PIÑAS", id)) %>%
  mutate(id = ifelse(id == "GENERAL ANTONIO ELIZALDE", "GNRAL. ANTONIO ELIZALDE", id)) %>%
  mutate(id = ifelse(id == "FRANCISCO DE ORELLANA", "ORELLANA", id)) 

# combine with map to get pcdoes
map <- read_sf("./maps/ecu_admbnda_adm2_inec_20190724.shp") %>%
  st_drop_geometry() %>%
  select(ADM2_PCODE, ADM2_ES) %>%
  mutate(id = toupper(ADM2_ES)) %>%
  select(-ADM2_ES)

df %<>%
  left_join(map)

df %<>%  mutate(ADM1_PCODE = substr(ADM2_PCODE, 1, 4)) %>%
# there are two Bolivar cantons and this one is mis-assigned to incorrect canton
              mutate(Provincia = ifelse(ADM2_PCODE == "EC0402", "CARCHI", Provincia)) %>%
# canton became part of Guayas in 2017  
              mutate(Provincia = ifelse(id == "EL PIEDRERO", "GUAYAS", Provincia), 
                     ADM1_PCODE = ifelse(id == "EL PIEDRERO", "EC09", ADM1_PCODE)) %>%
  # canton La Concordia voted to become part of Santo Domingo de los Tsachilas province
              mutate(ADM1_PCODE = ifelse(ADM2_PCODE == "EC0808", "EC23", ADM1_PCODE)) %>%
  # canton Olmedo is in Manabi, fix ADM1_PCODE
              mutate(ADM1_PCODE = ifelse(ADM2_PCODE == "EC1116", "EC13", ADM1_PCODE)) %>%
              rename(ADM2_PCODE_ignore = ADM2_PCODE)

write.csv(case_df, "../cases/ECU_weekly_cases.csv", row.names=FALSE)


