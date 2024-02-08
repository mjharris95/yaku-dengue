library(microsynth)
library(magrittr)
library(tidyverse)
library(lubridate)

case_files <- dir("cases/")
pop_files <- dir("pop/")
wea_files <- dir("weather-processed")
r0_files <- dir("r0-processed")

my_dates <- expand.grid(year = 2014:2022, week = 1:52) %>%
            filter(! (year %in% c(2014, 2015, 2018, 2019, 2020, 2021) & week==52)) %>%
            rbind(data.frame(year=2023, week=1:27))


full_df <- c()

for(country in c("PER", "MEX", "COL", "BRA", "ECU")){
  case_file <- case_files[startsWith(case_files, country)]
  wea_file <- wea_files[startsWith(wea_files, country)]
  pop_file <- pop_files[startsWith(pop_files, country)]
  r0_file <- r0_files[startsWith(r0_files, country)]
  
  # read in case data
  case_df <- read.csv(paste0("cases/", case_file)) 
  
  if(country %in% c("COL")){
    case_df %<>% group_by(ADM1_PCODE, year, week) %>%
                  summarize(cases = sum(cases))
  }
  
  adm_lev <- names(case_df)[endsWith(names(case_df), "PCODE")]
  
  case_df %<>% rename(name = !!adm_lev) %>%
          select(cases, week, year, name)
  
  # loop through all locations and make sure they contain an entry for each timepoint
  case_df_filled<- data.frame()
  
  for(place in unique(case_df$name)){
    case_df_filled <- filter(case_df, name == place) %>%
                      right_join(my_dates, by=c("week", "year")) %>%
                      mutate(cases = ifelse(is.na(cases), 0, cases),
                             name = place) %>% 
                      rbind(case_df_filled)
  }
  
  pop_df <- read.csv(paste0("pop/", pop_file)) %>%
              rename(name = !!adm_lev)
  
  if("sum" %in% names(pop_df)){
    pop_df %<>% rename(pop = sum)
  }
  
  case_df_filled <- pop_df %>%
    mutate(pop = round(pop, digits=0)) %>%
    right_join(case_df_filled) %>%
    mutate(inc = cases/pop)
  
  
  # read in, transform, append weather data
  df <- read.csv(paste0("D:/Attribution/weather-processed/", wea_file)) %>%
    pivot_wider(names_from=property, values_from=mean) %>%
    rename(name = !!adm_lev) %>%
    mutate(time = as.Date(date)) %>%
    mutate(week = epiweek(time), year = year(time)) %>%
    group_by(week, year, name) %>%
    summarize(mean_temp = mean(temperature_2m),
              precip = mean(total_precipitation_hourly*24)) %>%
    right_join(case_df_filled) %>%
    select(name, cases, pop, inc, week, year, mean_temp, precip) %>%
    mutate(country = country) 
  
  df <- read.csv(paste0("D:/Attribution/r0-processed/", r0_file)) %>%
    pivot_wider(names_from=property, values_from=mean) %>%
    rename(name = !!adm_lev) %>%
    rename(r0 = remapped) %>%
    mutate(time = as.Date(date)) %>%
    mutate(week = epiweek(time), year = year(time)) %>%
    group_by(week, year, name) %>%
    summarize(r0 = mean(r0)) %>%
    right_join(df) %>%
    mutate(name = as.character(name))
  
  full_df <- rbind(df, full_df)
}

# put in format required for microsynth package
extreme_prov <- c("PE14", "PE20", "PE24", "EC13", "EC24") 

# provinces to remove because they are above 80th percentile or have no cases
remove_prov <- c("EC01", "EC02", "EC03", "EC04",  "EC05", "EC06", "EC07",
  "EC08", "EC09", "EC11", "EC12", "EC14", "EC15",
  "EC16", "EC18", "EC19", "EC21", "EC22", "EC23",
  "EC90", "PE01", "PE13", "PE16", "PE22", "EC20",
  "MX09", "MX29", "PE20", "PE24", "EC13", "EC24")


full_df %<>%
  mutate(time = as.Date(paste(year, week, 7, sep="-"), "%Y-%U-%u")) %>%
  mutate(time = as.numeric(time)) %>%
  mutate(int = ifelse(name %in% extreme_prov & time >= as.Date("2023-03-07"), 1, 0)) %>%
  group_by(name, pop, week, year, mean_temp, r0, precip, country, time, int) %>%
  summarize(cases = sum(cases)) %>%
  mutate(inc = cases / pop) %>%
  mutate(time=as.numeric(time)) %>%
  ungroup()

# weird stuff to aggregate to 4-week periods like we're doing for synthetic control
times <- unique(full_df$time)
time_df <- data.frame(time = sort(times), ind = 1:length(times)) %>%
  mutate(step=ceiling(ind/4)) 

full_df %<>% left_join(time_df) %>%
  group_by(name, step, pop) %>%
  summarize(cases = sum(cases),
            inc = sum(inc),
            mean_temp = mean(mean_temp),
            precip = mean(precip),
            r0 = mean(r0),
            int = ifelse(sum(int)>0, 1, 0),
            time = max(time),
            year = max(year),
            week= max(week)) %>%
  mutate(log_cases = log(cases + 1)) %>%
  filter(step < 123)

save(full_df, file="global-forsynth.RData") 
load('global-forsynth.RData')
