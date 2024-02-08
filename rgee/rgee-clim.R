library(rgee)
library(tidyverse)

#ee = import("ee")
ee_Initialize(user = "malljes13",
              project="ee-dengue-climate")

gunix_to_date <- function(unix_timestamp) {
  return(as.POSIXct(unix_timestamp / 1000, origin = "1970-01-01", tz = "UTC"))
}

country <- "MEX"

shape <- switch(   
  country,   
  "PER"= ee$FeatureCollection("projects/ee-dengue-climate/assets/per_admbnda_adm1_ign_20200714"),   
  "BRA"= ee$FeatureCollection("projects/ee-dengue-climate/assets/bra_admbnda_adm1_ibge_2020"),   
  "MEX"= ee$FeatureCollection("projects/ee-dengue-climate/assets/mex_admbnda_adm1_govmex_20210618"),
  "ECU" = ee$FeatureCollection("projects/ee-dengue-climate/assets/ecu_admbnda_adm1_inec_20190724"),
  "COL" = ee$FeatureCollection("projects/ee-dengue-climate/assets/col_admbnda_adm1_mgn_20200416"))   


proj_start = "1993-01-01"  
proj_end = "2023-09-01" 

proj_start <- ifelse(country %in% c("PER", "ECU"), "1993-01-01", "2013-01-01")
n_date_breaks <- ifelse(country %in% c("PER", "BRA", "ECU"), 120, 40)


proj_date_seq <- seq.Date(as.Date(proj_start, format = "%Y-%m-%d"),
                                            as.Date(proj_end, format = "%Y-%m-%d"),
                                            by = "1 days") %>% 
  {.[c(1, cut(1:length(.), n_date_breaks) %>% table %>% as.numeric %>% cumsum)]}
  
exportTasks<-list()

# pull 1970-2000 data for worldclim and era5 (will be used in debiasing)
ee_era5_clim_loc <- "users/marissachilds/era5_monthly_climatology"

worldclim_clim <- purrr::map(1:12, 
                             function(m){
                               ee$Image(paste0("users/lyberger/worldclim", 
                                               "/", 
                                               month.name[m])) %>% 
                                 ee$Image$set("month", m)
                             }) %>% ee$List()

era5_clim <- purrr::map(1:12, 
                        function(m){
                          ee$Image(paste0(ee_era5_clim_loc, 
                                          "/era5_climatology_1970-2000_", 
                                          formatC(m, width=2, flag="0"))) %>% 
                            ee$Image$set("month", m)
                        }) %>% ee$List()

pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_proj <- pop_orig$first()$projection()
pop_scale <- pop_proj$nominalScale()$getInfo()


# get the correct month for a the given month
# subtract/add stuff
failed_units <- 1:n_date_breaks

for(i in failed_units){
  startDate <- format(proj_date_seq[i], format = "%Y-%m-%d")  # Format as string
  endDate <- format(proj_date_seq[i + 1], format = "%Y-%m-%d")  # Format as string
  
  
  startDate <- ee$Date(startDate)
  endDate <- ee$Date(endDate)
  
  era5 <- ee$ImageCollection("ECMWF/ERA5_LAND/HOURLY")$
    filter(ee$Filter$date(startDate, endDate))$
    select(c("temperature_2m", "total_precipitation_hourly"))
  
  # Calculate the number of days between startDate and endDate
  n_days <- floor(as.numeric(proj_date_seq[i+1]) - as.numeric(proj_date_seq[i]))-1
  
  # Create a list of days from 0 to n_days
  days <- seq(0, n_days, by=1)
  
  # get population and population density
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
    end <- endDate$advance(day+1, 'day')
    
    # Filter the ERA5 ImageCollection for the current day
    filtered <- era5$filter(ee$Filter$date(start, end))
    
    # Calculate the mean of the filtered ImageCollection
    filtered_mean <- filtered$mean()
    
  
    # debias using monthly climatologies
    my_month <- start$get("month")$subtract(1)
    
    filtered_mean <-  filtered_mean$select("temperature_2m") %>% 
      ee$Image$subtract(era5_clim$get(my_month)) %>%
      ee$Image$add(worldclim_clim$get(my_month)) %>%
      # also add DTR and precip average bands
      ee$Image$addBands(filtered_mean$select(list("total_precipitation_hourly")))
    
    # Set system:time_start and system:time_end
    filtered_mean <- filtered_mean$
      set('day', day)$
      set('system:time_start', start)$
      set('system:time_end', end)
      
    filtered_mean$bandNames()$map(ee_utils_pyfunc(function(band_name){
        filtered_mean$select(list(band_name))$addBands(pop_density) %>% 
          ee$Image$reduceRegions(
            collection = shape,
            reducer = ee$Reducer$mean()$splitWeights(),
            scale = 1000,
            crs = 'EPSG:4326' 
          ) %>% 
          ee$FeatureCollection$map(function(feature) {
            return(feature$set('date', filtered_mean$get('system:time_start'),
                               "property", ee$String(band_name)))
          })
      }))  %>% ee$FeatureCollection() %>%
               ee$FeatureCollection$flatten() 
    
    })  %>% ee$FeatureCollection() %>%
    ee$FeatureCollection$flatten()
  
    export_temp_task <- ee_table_to_drive(
      collection = daily,
      description = paste0(country,'_adm1_daily-', i),
      fileFormat = 'CSV',
      selectors = c("ADM1_PCODE", "mean", "property", "date")
    ) 
    
    export_temp_task$start()
}
  

  

