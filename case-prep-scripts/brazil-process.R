library(tidyverse)
library(magrittr)

### FOR ADM1 ###
bra_files <- dir("../brazil-adm1-cases")
full_df <- c()
years<-c()

# loop through all filess
for(file in bra_files){
  # identify the year
  my_year <- read.table(paste0("brazil-adm1-cases/",file), skip=2, 
                        sep=";", nrows=1, header=FALSE, encoding="UTF-8") %>%
    unlist() %>%
    str_split(., pattern=":") %>%
    unlist() %>%
    tail(1)

  # grab the rows that are the table
  df <- read.csv(paste0("brazil-adm1-cases/",file), skip=3, sep=";",
                 header=TRUE, stringsAsFactors = FALSE, check.names = F,
                 encoding="UTF-8",colClasses = "character")  %>%
    rename("id" = 1) %>%
    select(-"Total") %>%
    select(starts_with(c("Semana","id"))) %>%
    filter(`Semana 01` != "" & id != "Total" & !is.na(`Semana 01`)) %>%
    rowwise() %>%
    mutate(id = str_split(id, " ", simplify=TRUE)[1]) %>%
    # make week into a column
    pivot_longer(col=starts_with("Semana "),
                 names_to = "week",
                 names_prefix= "Semana ",
                 values_to = "cases") %>%
    # make a year row
    mutate(year = my_year) %>%
    mutate(cases = ifelse(cases=="-", 0, cases)) %>%
    mutate(ADM1_PCODE = paste0("BR", id))
  
  
  full_df <- rbind(df, full_df)
}

full_df %<>% filter(ADM1_PCODE != "BR00")

write.csv(full_df, "../cases/BRA_adm1_weekly_cases.csv", row.names=FALSE)
