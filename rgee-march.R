#library(reticulate)
#use_condaenv("rgee", conda="auto", required = TRUE)

library(rgee)
library(tidyverse)

ee = import("ee")
ee_Initialize(user = "malljes13", # saves to my personal Google Earth Engine project, set for different users
              project="ee-dengue-climate")

# read in temperature-dependent R0
aedes_r0 <- read.csv("AedesR0Out.csv")
temps_list <- ee$List(aedes_r0$temperature)
aedesr0_list <- ee$List(aedes_r0$aegypti.R0.median)

# submit runs for different countries separately.  Set country here to submit a run.
# options are PER (Peru, admin1), PER3 (Peru, adm3), BRA (Brazil, admin2),
# ECU1 (Ecuador, admin1), ECU2 (Ecuador, adm2). Only PER3 and PER are used in manuscript.
country <- "PER"

shape <- switch(   
  country,   
  "PER"= ee$FeatureCollection("projects/ee-dengue-climate/assets/CDC_Departamentos"),
  "PER3"= ee$FeatureCollection("projects/ee-dengue-climate/assets/CDC_Distritos"),
  "ECU1"= ee$FeatureCollection("projects/ee-dengue-climate/assets/ecu_admbnda_adm1_inec_20190724"),
  "ECU2" = ee$FeatureCollection("projects/ee-dengue-climate/assets/ecu_admbnda_adm2_inec_20190724"))   

# specify the column containing the unit id
adm_lev <- switch(   
  country,   
  "PER"= "DEPARTAMEN",
  "PER3"= "ubigeo",
  "ECU1" = "ADM1_PCODE",
  "ECU2" = "ADM2_PCODE")   

# will extract for march from 1973-2023
years<-1973:2023

start_dates <- as.Date(paste0(years, "-03-01"), format = "%Y-%m-%d")
end_dates <- as.Date(paste0(years, "-04-01"), format = "%Y-%m-%d")

exportTasks<-list()

# pull 1970-2000 data for worldclim and era5 (will be used in debiasing)
# these directories are not public, need to be directly shared
ee_era5_clim_loc <- "users/marissachilds/era5_monthly_climatology"

# monthly averages for worldclim
worldclim_clim <- purrr::map(1:12, 
                             function(m){
                               ee$Image(paste0("users/lyberger/worldclim", 
                                               "/", 
                                               month.name[m])) %>% 
                                 ee$Image$set("month", m)
                             }) %>% ee$List()

# monthly averages for ERA5
era5_clim <- purrr::map(1:12, 
                        function(m){
                          ee$Image(paste0(ee_era5_clim_loc, 
                                          "/era5_climatology_1970-2000_", 
                                          formatC(m, width=2, flag="0"))) %>% 
                            ee$Image$set("month", m)
                        }) %>% ee$List()

# extract population from WorldPop
pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_proj <- pop_orig$first()$projection()
pop_scale <- pop_proj$nominalScale()$getInfo()


# if rerunning, set to index of years that failed. Otherwise, 1:length(years)
# can use output dataframe names to determine missing indices

failed_years <- 1:length(years)
for(i in failed_years){
  # time range to extract for
  startDate <- format(start_dates[i], format = "%Y-%m-%d")  # Format as string
  endDate <- format(end_dates[i], format = "%Y-%m-%d")  # Format as string
  
  startDate <- ee$Date(startDate)
  endDate <- ee$Date(endDate)
  
  # select the desired climate variables over the specified time period
  era5 <- ee$ImageCollection("ECMWF/ERA5_LAND/HOURLY")$
    filter(ee$Filter$date(startDate, endDate))$
    select(c("temperature_2m", "total_precipitation_hourly"))
  
  # Calculate the number of days between startDate and endDate
  n_days <- floor(as.numeric(end_dates[i]) - as.numeric(start_dates[i]))-1
  
  # Create a list of days from 0 to n_days
  days <- seq(0, n_days, by=1)
  
  # get population and population density (for 2020)
  pop <- pop_orig %>% 
    ee$ImageCollection$filter(ee$Filter$calendarRange(2020, 2020, "year")) %>%
    ee$ImageCollection$reduce(ee$Reducer$mean())
  
  pop_density <- pop_orig %>% 
    ee$ImageCollection$filter(ee$Filter$calendarRange(2020, 2020, "year")) %>%
    ee$ImageCollection$reduce(ee$Reducer$mean()) %>% 
    ee$Image$divide(ee$Image$pixelArea())
  
  # Create a daily ImageCollection
  daily <-sapply(days, function(day) {
    start <- startDate$advance(day, 'day')
    end <- startDate$advance(day+1, 'day')
    
    # Filter the ERA5 ImageCollection for the current day
    filtered <- era5$filter(ee$Filter$date(start, end))
    
    # debias daily climate using monthly climatologies
    my_month <- start$get("month")$subtract(1)
    
    filtered_demean <- filtered$map(function(image){
      image %>% 
        ee$Image$subtract(era5_clim$get(my_month)) %>%
        ee$Image$add(worldclim_clim$get(my_month)) 
    })
    
    # round to nearest tenth (to convert to temperature-dependent R0)
    filtered_round <- filtered_demean$map(function(image){
      image$select("temperature_2m")$multiply(10)$round()$divide(10) 
    })
    
    # map from temperature to temperature-dependent R0
    filtered_r0 <-  filtered_round$map(function(image){
      image$select("temperature_2m") %>% ee$Image$remap(from = temps_list, to = aedesr0_list, defaultValue = 0)
    })
    
    # Calculate the means of the filtered ImageCollections
    filtered_mean_r0 <- filtered_r0$mean()
    filtered_mean_clim <- filtered$mean()
    
    # debias mean temperature using monthly climatologies
    my_month <- start$get("month")$subtract(1)
    filtered_mean <-  filtered_mean_clim$select("temperature_2m") %>% 
      ee$Image$subtract(era5_clim$get(my_month)) %>%
      ee$Image$add(worldclim_clim$get(my_month)) %>%
      # also add temperature dependent R0 and precip average bands
      ee$Image$addBands(filtered_mean_clim$select(list("total_precipitation_hourly"))) %>%
      ee$Image$addBands(filtered_mean_r0$select(list("remapped")))
    
    # Set system:time_start and system:time_end
    filtered_mean <- filtered_mean$
      set('day', day)$
      set('system:time_start', start)$
      set('system:time_end', end)
    
    
    
    filtered_mean$bandNames()$map(ee_utils_pyfunc(function(band_name){
      filtered_mean$select(list(band_name))$addBands(pop_density) %>% 
        ee$Image$reduceRegions(
          collection = shape, # take average across each spatial unit
          reducer = ee$Reducer$mean()$splitWeights(), # population-weighting by pop_density
          scale = 1000,
          crs = 'EPSG:4326' 
        ) %>% 
        ee$FeatureCollection$map(function(feature) {
          return(feature$set('date', filtered_mean$get('system:time_start'),
                             'end_date', filtered_mean$get('system:time_end'),
                             "property", ee$String(band_name)))
        })
    }))  %>% ee$FeatureCollection() %>% # convert to a table
      ee$FeatureCollection$flatten()  
    
  })  %>% ee$FeatureCollection() %>%
    ee$FeatureCollection$flatten()
  
  # output to Google Drive
  export_temp_task <- ee_table_to_drive(
    collection = daily,
    description = paste0(country,'-daily_clim-marchtest', i), # file name will be formatted in this manner
    fileFormat = 'CSV',
    selectors = c(adm_lev, "mean", "property", "date", "end_date")
  ) 
  
  
  export_temp_task$start()
  
}



