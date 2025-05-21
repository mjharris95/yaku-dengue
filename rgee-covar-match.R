library(rgee)
library(tidyverse)

#ee = import("ee")
ee_Initialize(user = "malljes13",  # saves to my personal Google Earth Engine project, set for different users
              project="ee-dengue-climate")

# read in shapefile
shape <- ee$FeatureCollection("projects/ee-dengue-climate/assets/CDC_Distritos")
adm_lev <- "ubigeo"

exportTasks<-list()

# extract population from WorldPop
pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_proj <- pop_orig$first()$projection()
pop_scale <- pop_proj$nominalScale()$getInfo()

pop_density <- pop_orig %>% 
  ee$ImageCollection$filter(ee$Filter$calendarRange(2020, 2020, "year")) %>%
  ee$ImageCollection$reduce(ee$Reducer$mean()) %>% 
  ee$Image$divide(ee$Image$pixelArea())
  
 
dpop <- pop_density$addBands(pop_density) %>%
  ee$Image$reduceRegions(
    collection = shape, # take average across each spatial unit
    reducer = ee$Reducer$mean()$splitWeights(), # population-weighting by pop_density
    crs = pop_proj,
    scale = pop_scale
  )


export_dpop_task <- ee_table_to_drive(
  collection = dpop,
  description = "match-sens_covar",
  fileFormat = "CSV",
  selectors = list(adm_lev, "mean")
)

export_dpop_task$start()

    