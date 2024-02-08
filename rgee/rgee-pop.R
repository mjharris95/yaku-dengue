library(rgee)
library(tidyverse)

#ee = import("ee")
ee_Initialize(user = "malljes13")

gunix_to_date <- function(unix_timestamp) {
  return(as.POSIXct(unix_timestamp / 1000, origin = "1970-01-01", tz = "UTC"))
}

#shape <- ee$FeatureCollection("projects/ee-dengue-climate/assets/per_admbnda_adm2_ign_20200714") 
#shape <- ee$FeatureCollection("projects/ee-dengue-climate/assets/per_admbnda_adm1_ign_20200714") 
#shape <- ee$FeatureCollection("projects/ee-dengue-climate/assets/col_admbnda_adm2_mgn_20200416") 
#shape <- ee$FeatureCollection("projects/ee-dengue-climate/assets/bra_admbnda_adm2_ibge_2020") 
#shape <- ee$FeatureCollection("projects/ee-dengue-climate/assets/bra_admbnda_adm1_ibge_2020") 
#shape <- ee$FeatureCollection("projects/ee-dengue-climate/assets/mex_admbnda_adm1_govmex_20210618") 
shape <- ee$FeatureCollection("projects/ee-dengue-climate/assets/ecu_admbnda_adm2_inec_20190724") 

exportTasks<-list()
pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_proj <- pop_orig$first()$projection()
pop_scale <- pop_proj$nominalScale()$getInfo()

countries <- c("ECU1", "COL1")
for(country in countries){

  shape <- switch(   
    country,   
    "PER"= ee$FeatureCollection("projects/ee-dengue-climate/assets/per_admbnda_adm1_ign_20200714"),   
    "BRA"= ee$FeatureCollection("projects/ee-dengue-climate/assets/bra_admbnda_adm1_ibge_2020"),   
    "COL"= ee$FeatureCollection("projects/ee-dengue-climate/assets/col_admbnda_adm2_mgn_20200416"),   
    "ECU"= ee$FeatureCollection("projects/ee-dengue-climate/assets/ecu_admbnda_adm2_inec_20190724"), 
    "MEX"= ee$FeatureCollection("projects/ee-dengue-climate/assets/mex_admbnda_adm1_govmex_20210618"),
    "ECU1" = ee$FeatureCollection("projects/ee-dengue-climate/assets/ecu_admbnda_adm1_inec_20190724"),
    "COL1" = ee$FeatureCollection("projects/ee-dengue-climate/assets/col_admbnda_adm1_mgn_20200416"))   
  
  adm_lev <- switch(
    country,
    "PER" = "ADM1_PCODE",
    "BRA" = "ADM1_PCODE",
    "COL" = "ADM2_PCODE",
    "ECU" = "ADM2_PCODE",
    "MEX" = "ADM1_PCODE",
    "ECU1" = "ADM1_PCODE",
    "COL1" = "ADM1_PCODE"
  )
  
  pop <- pop_orig %>% 
    ee$ImageCollection$filter(ee$Filter$calendarRange(2020, 2020, "year")) %>%
    ee$ImageCollection$reduce(ee$Reducer$mean())
  
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
  
  exportTasks[[country]] <- export_pop_task
}


for(country in countries){
  exportTasks[[country]]$start()
}

