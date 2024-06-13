library(readxl)
library(magrittr)
library(tidyverse)
library(sf)

# read in and combine files for Colombia
df <- data.frame()
  
for(year in 2013:2023){
  # get the correct sheet
  if(year <= 2016){
    sheet_name <- paste0("rutinaria_", year)
  }
  else if (year == 2017){
    sheet_name <- "RUTINARIA"
  }
  else if (year == 2018){
    sheet_name <- "Metadatos"
  }

  if(year <= 2018){
    df <- read_xlsx(paste0("colombia_data/rutinaria_", year, ".xlsx"), sheet=sheet_name) %>%
          rename(any_of(c("Nombre" = "Evento",
                          "ADM2_PCODE" = "Cod_municipio",
                          "ADM1_PCODE" = "COD_DPTO",
                          "ADM2_PCODE" = "cod_mun",
                          "ADM1_PCODE" = "cod_dep",
                          "ADM2_PCODE" = "cod_municipio",
                          "ADM1_PCODE" = "Cod_departamento"))) %>%
          filter(Nombre == "DENGUE") %>%
          mutate(year = year) %>%
          select(SEMANA, year, conteo, Departamento, Municipio, ADM2_PCODE, ADM1_PCODE) %>%
          rename(week = SEMANA,
                 cases = conteo,
                 id = Municipio) %>%
        rbind(df)
  }
  
  else{
    #spreadsheets after 2018 are formatted differently
    if(year == 2019){
      sheet_name <- "Municipal"
      skip_num <- 1
    }
    else if(year == 2020){
      sheet_name <- "Por municipios"
      skip_num <- 3
    }
    else if(year == 2021){
      sheet_name <- "Municipio"
      skip_num <- 1
    }
    else if(year == 2022){
      sheet_name <- "Municipio"
      skip_num <- 0
    }
    
    year_df <- read_excel(paste0("colombia_data/rutinaria_", year, ".xlsx"), sheet=sheet_name, skip=skip_num)
    
    if(year <=2021){
      colnames(year_df) <- year_df[1,]
      year_df %<>% tail(-1) 
    }

    # columns are inconsistently named
    year_df %<>%  rename(any_of(c("NOM_EVE" = "Evento",
                                  "NOM_EVE" = "Nombre del evento",
                                "NDEP_PROCE" = "Nombre departamento",
                                "NMUN_PROCE" = "Nombre Municipio",
                                "NDEP_PROCE" = "Nombre del departamento",
                                "NMUN_PROCE" = "Nombre del municipio",
                                "ADM2_PCODE" = "COD_MUN_O",
                                "ADM1_PCODE" = "COD_DPTO_O",
                                "ADM1_PCODE" = "Código Departamento",
                                "ADM2_PCODE" = "Código Municipio",
                                "ADM1_PCODE" = "Codigo del departamento",
                                "ADM2_PCODE" = "Codigo del municipio")))  
    
    # get the total cases and pivot so each week is a row
    year_df %<>% fill(NDEP_PROCE, NMUN_PROCE, ADM2_PCODE, ADM1_PCODE) %>%
            filter(NOM_EVE == "DENGUE") %>%
            filter(! startsWith(NMUN_PROCE, "Total")) %>%
            rename_at(vars(starts_with('Semana')), toupper) %>%
            pivot_longer(cols = starts_with("SEMANA_"),
              names_to = "week",
              names_prefix = "SEMANA_",
              values_to = "cases"
            ) %>%
            mutate(year = year) %>%
            select(week, year, cases, "NDEP_PROCE", "NMUN_PROCE", "ADM2_PCODE", "ADM1_PCODE") %>%
            rename(id = NDEP_PROCE,
                   Municipio = NMUN_PROCE) 
    
    # Municipality codes are store differently for 2019-2021
    if(year %in% c(2019, 2020, 2021)){
      #get back leading zeroes for 2021
      if(year == 2021){
        year_df %<>% mutate(ADM2_PCODE = str_pad(side="left", width=3, pad="0", ADM2_PCODE),
                               ADM1_PCODE = str_pad(side="left", width=2, pad="0", ADM1_PCODE)) 
      }
      
      year_df %<>%
        mutate(ADM2_PCODE = paste0(ADM1_PCODE,ADM2_PCODE))
    }
    
    df %<>% rbind(year_df)
    
  }
  
  
  # take substrings to get the ADM1 and ADM2 pcodes
  df %<>% 
    mutate(week = as.numeric(week)) %>%
    mutate(ADM2_PCODE = str_sub(ADM2_PCODE, start= -5),
           ADM1_PCODE = str_sub(ADM1_PCODE, start= -2)) %>%
    mutate(ADM2_PCODE = paste0("CO", ADM2_PCODE),
           ADM1_PCODE = paste0("CO", ADM1_PCODE))
}

# for now, we'll ignore the adm2 pcode
df %<>% rename(ADM2_PCODE_IGNORE = ADM2_PCODE)

# df %<>% group_by(week, id, year) %>%
#   summarize(cases = sum(cases, na.rm=TRUE))

write.csv(df, "../cases/COL_cases.csv")

