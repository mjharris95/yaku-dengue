library(rgee)
library(tidyverse)

#ee = import("ee")
ee_Initialize(user = "malljes13",  # saves to my personal Google Earth Engine project, set for different users
              project="ee-dengue-climate")

# read in temperature-dependent R0
aedes_r0 <- read.csv("AedesR0Out.csv")
temps_list <- ee$List(aedes_r0$temperature)
aedesr0_list <- ee$List(aedes_r0$aegypti.R0.median)

country <- "PER3"

# submit runs for different countries separately.  Set country here to submit a run.
# options are PER (Peru, admin1), PER3 (Peru, adm3), BRA (Brazil, admin1), MEX (Mexico, admin1),
# ECU1 (Ecuador, admin1), ECU2 (Ecuador, adm2), COL (Colombia, admin2)
shape <- switch(   
  country,   
  "PER"= ee$FeatureCollection("projects/ee-dengue-climate/assets/CDC_Departamentos"),
  "PER3"= ee$FeatureCollection("projects/ee-dengue-climate/assets/CDC_Distritos"),
  "BRA"= ee$FeatureCollection("projects/ee-dengue-climate/assets/bra_admbnda_adm1_ibge_2020"),
  "MEX"= ee$FeatureCollection("projects/ee-dengue-climate/assets/mex_admbnda_adm1_govmex_20210618"),
  "ECU1"= ee$FeatureCollection("projects/ee-dengue-climate/assets/ecu_admbnda_adm1_inec_20190724"),
  "ECU2" = ee$FeatureCollection("projects/ee-dengue-climate/assets/ecu_admbnda_adm2_inec_20190724"),
  "COL" = ee$FeatureCollection("projects/ee-dengue-climate/assets/col_admbnda_adm1_mgn_20200416"))   


# specify the column containing the unit id
adm_lev <- switch(   
  country,   
  "PER"= "DEPARTAMEN",
  "PER3"= "ubigeo",
  "BRA" = "ADM1_PCODE",
  "MEX"= "ADM1_PCODE",
  "ECU1" = "ADM1_PCODE",
  "ECU2" = "ADM2_PCODE",
  "COL" = "ADM1_PCODE")   

# extract climate covariates from 2018 through the end of 2023 (change to 2009 start if desired for Peru districts)
proj_start = "2009-06-01" # get a few extra months for appropriate climate lags
proj_end <- "2023-12-31"

# defines how many chunks to break the time series into -> more, smaller chunks may run more quickly 
n_date_breaks <- ifelse(country %in% c("PER3", "BRA2", "ECU2"), 30, 50) # can play around with these values


# break time period into intervals based on n_date_breaks
proj_date_seq <- seq.Date(as.Date(proj_start, format = "%Y-%m-%d"),
                          as.Date(proj_end, format = "%Y-%m-%d"),
                          by = "1 days") %>% 
  {.[c(1, cut(1:length(.), n_date_breaks) %>% table %>% as.numeric %>% cumsum)]}

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

# if rerunning, set to index of years that failed. Otherwise, 1:n_date_breaks
# can use output dataframe names to determine missing indices
failed_units <- 1:n_date_breaks

for(i in failed_units){
  # time range to extract for
  startDate <- format(proj_date_seq[i], format = "%Y-%m-%d")  # Format as string
  endDate <- format(proj_date_seq[i + 1], format = "%Y-%m-%d")  # Format as string
  
  
  startDate <- ee$Date(startDate)
  endDate <- ee$Date(endDate)
  
  # select the desired climate variables over the specified time period
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
    end <- startDate$advance(day+1, 'day')
    
    # Filter the ERA5 ImageCollection for the current day
    filtered <- era5$filter(ee$Filter$date(start, end))
    
    # debias daily values using monthly climatologies
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
    
    # Calculate the mean of the filtered ImageCollection
    filtered_mean_r0 <- filtered_r0$mean()
    filtered_mean_clim <- filtered$mean()
    
    # debias using monthly climatologies
    my_month <- start$get("month")$subtract(1)
    
    # debias mean temperature using monthly climatologies
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
                             "property", ee$String(band_name)))
        })
    }))  %>% ee$FeatureCollection() %>%
      ee$FeatureCollection$flatten() 
    
  })  %>% ee$FeatureCollection() %>%
    ee$FeatureCollection$flatten()
  
  export_temp_task <- ee_table_to_drive(
    collection = daily,
    description = paste0(country,'-daily_clim-', i),  # file name will be formatted in this manner
    fileFormat = 'CSV',
    selectors = c(adm_lev, "mean", "property", "date")
  ) 
  
  export_temp_task$start()
  
  # output to Google Drive
  if(i == 1){    # when running for the first year, also export population
    muni_pop <- pop %>% ee$Image$reduceRegions(collection = shape,
                                               reducer = ee$Reducer$sum(),
                                               crs = pop_proj,
                                               scale = pop_scale)

    export_pop_task <- ee_table_to_drive(
      collection = muni_pop,
      description = paste0(country,
                           "_pop"),
      fileFormat = "CSV",
      selectors = list(adm_lev, "sum")
    )

    export_pop_task$start()
  }

}




