# process and merge Google Earth Engine files 

# March climate from 1993 - 2023
files <- dir("/march-files") # assumes all of the March files into a folder called "march-files"

march_df <- data.frame()

for(file in files){
  my_country <- str_split(file, "-")[[1]][1]
  
  march_df <- read.csv(paste0("march-files/", file)) %>%
    rename(id = 1) %>%
    pivot_wider(names_from = property,
                values_from = mean) %>%
    rename(rain = total_precipitation_hourly,
           temp = temperature_2m) %>%
    mutate(rain = rain * 24) %>% # convert hourly value to daily
    mutate(country = my_country) %>%
    select(id, rain, temp, date, country) %>%
    rbind(march_df)
}

write.csv(march_df, "march-clim_df.csv", row.names=FALSE)

# Climate from 2016 to 2023 (used for matching)
files <- dir("/clim-tomatch") # assumes all of the rgee files for matching are in a folder called "clim-tomatch"

df <- data.frame()

for(file in files){
  my_country <- str_split(file, "-")[[1]][1]
  
  df <- read.csv(paste0("clim-tomatch/", file)) %>%
    rename(id = 1) %>%
    pivot_wider(names_from = property,
                values_from = mean) %>%
    rename(rel_r0 = remapped,
           rain = total_precipitation_hourly,
           temp = temperature_2m) %>%
    mutate(rain = rain * 24) %>%
    mutate(country = my_country) %>%
    select(id, rel_r0, rain, temp, date, country) %>%
    rbind(df)
}


df$id[df$country=="PER3"]<-str_pad(df$id[df$country=="PER3"], 6, pad="0") # reformat ubigeos for Peru districts to be compatible with case data
df %>% select(country, id) %>% distinct() %>% group_by(country) %>% summarize(frq=n()) # how many units are in each country?
fwrite(df %>% distinct(), "clim_df.csv", row.names=FALSE)


# combine adm1 case data
case_files <- dir("cases/")
pop_files <- dir("pop/")
full_df <- c()

# case data 
for(country in c("PER", "MEX", "COL", "BRA", "ECU")){
  case_file <- case_files[startsWith(case_files, country)]
  if(length(case_file) == 2){
    case_file <- "PER_pop.csv"
  }
  
  # read in case data
  case_df <- read.csv(paste0("cases/", case_file)) 
  
  if(country %in% c("COL")){
    case_df %<>% group_by(ADM1_PCODE, year, week) %>%
      summarize(cases = sum(cases))
  }
  
  if(country != "PER"){
    adm_lev <- names(case_df)[endsWith(names(case_df), "PCODE")]
    
    case_df %<>% rename(name = !!adm_lev) %>%
      select(cases, week, year, name) %>%
      mutate(country=country) %>%
      rename(id = name)
  } else {
    case_df %<>% 
      select(cases, week, year, id) %>%
      mutate(country = country)            
  }
  
  pop_file <- pop_files[startsWith(pop_files, country)]
  pop_df <- read.csv(paste0("pop/", pop_file)) %>%
    rename(id = 1, 
           pop = sum)
  
  case_df %<>% left_join(pop_df)
  
  full_df <- rbind(case_df, full_df)
}
write.csv(full_df, "adm1_cases.csv", row.names=FALSE)

