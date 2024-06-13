library(readxl)
library(tidyverse)
library(magrittr)

df <- data.frame()

for(year in as.character(2021:2023)){
  # read in one year
  year_df <- read_excel("mexico2020on.xlsx", sheet=year, col_names=c("Week", "Counts")) %>%
  # fill out weeks
    fill(Week) %>%
    rowwise() %>%
  # extract province name (have to use regex bc some province names are multiple words)
    mutate(id = str_split(Counts, pattern="(?<=[:alpha:])\\s(?=[\\d-])", simplify=TRUE)[1],
           CountList = str_split(Counts, pattern="(?<=[:alpha:])\\s(?=[\\d-])", simplify=TRUE)[2]) %>%
  # extract cases from start of numeric columns
    mutate(cases = str_split(CountList, " ", simplify=TRUE)[1]) %>%
    ungroup() %>%
  # add year column
    mutate(year = year) %>%
    rename(week = Week)

 df %<>% rbind(year_df)
 
 if(year == "2023"){
   # 2023 data is different (one row per year, so need additional step to reshape to one row per province-week)
   raw_df <- read_excel("mexico2020on.xlsx", sheet=year, col_names=c("Week", "Counts")) 
   year_df <- lapply(raw_df$Counts, function(x) str_split(x, pattern="(?<=[\\d-])\\s(?=[:alpha:])")) %>%
                unlist()  %>%
             data.frame("Counts" = ., "week" = rep(1:49, each=32))  %>%
             rowwise() %>%
             # extract province name (have to use regex bc some province names are multiple words)
             mutate(id = str_split(Counts, pattern="(?<=[:alpha:])\\s(?=[\\d-])", simplify=TRUE)[1],
                    CountList = str_split(Counts, pattern="(?<=[:alpha:])\\s(?=[\\d-])", simplify=TRUE)[2]) %>%
             # extract cases from start of numeric columns
             mutate(cases = str_split(CountList, " ", simplify=TRUE)[1]) %>%
             ungroup() %>%
             # add year column
             mutate(year = year) 
             
     df %<>% rbind(year_df)
 }

}

# read in 2003 - 2020 data
df <- read.csv("Mexico_data_subset03-20_v2.csv") %>%
          rename(week = semana,
                 id = ADM1_ES,
                 cases = CountValue,
                 year = Year) %>%
          rbind(df %>% select(week, cases, id, year)) %>%
          # Distrito Federal & Ciudad de Mexico are the same
          mutate(id = ifelse(id == "Ciudad de México", "Distrito Federal", id)) %>%
          # make numeric (note: changing some NAs to zeroes)
          mutate(cases = ifelse(cases %in% c("-", "", "-.", "n.e.", NA), 0, as.numeric(cases))) %>%
          distinct()

# change id to match map file
df %<>%
  mutate(id = ifelse(id == "Ciudad de México", "Distrito Federal", id)) %>%
  mutate(id = ifelse(id == "Coahuila", "Coahuila de Zaragoza", id)) %>%
  mutate(id = ifelse(id == "Michoacán", "Michoacán de Ocampo", id)) %>%
  mutate(id = ifelse(id == "Querétaro", "Querétaro de Arteaga", id)) %>%
  mutate(id = ifelse(id == "Veracruz", "Veracruz de Ignacio de la Llave", id)) 

map <- read_sf("../maps/mex_admbnda_adm1_govmex_20210618.shp") %>%
  st_drop_geometry() %>%
  rename(id = ADM1_ES) %>%
  select(id, ADM1_PCODE)

df %<>% left_join(map)

write.csv(df, "../cases/MEX_cases.csv", row.names=FALSE)
