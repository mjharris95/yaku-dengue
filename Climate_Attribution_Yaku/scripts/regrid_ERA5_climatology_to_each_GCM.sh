#!/bin/bash

######################################################################################################
## Regrid the ERA5 climatology to match each climate model grid in this analysis
## - this script uses Climate Data Operators remapbil function (Schulzweida, Uwe. 2023) 
## - Schulzweida, Uwe. (2023). CDO User Guide (2.3.0). Zenodo. https://doi.org/10.5281/zenodo.10020800
######################################################################################################

path_to_CMIP6_data='' # add path to directory containing CMIP6 data

f_grid=$path_to_CMIP6_data'/tas_Amon_ACCESS-ESM1-5_historical_r10i1p1f1_gn_185001-201412.nc'
f_ERA5='../processed_data/ERA5_25km_tas_climatology.nc'
f_out=${f_ERA5/.nc/_ACCESS-ESM1-5.nc}
f_out=${f_out/25km/GCM_grid}
echo $f_out
cdo -L remapbil,$f_grid $f_ERA5 $f_out

f_grid=$path_to_CMIP6_data'/tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc'
f_ERA5='../processed_data/ERA5_25km_tas_climatology.nc'
f_out=${f_ERA5/.nc/_CanESM5.nc}
f_out=${f_out/25km/GCM_grid}
echo $f_out
cdo -L remapbil,$f_grid $f_ERA5 $f_out

f_grid=$path_to_CMIP6_data'/tas_Amon_CNRM-CM6-1_historical_r10i1p1f2_gr_185001-201412.nc'
f_ERA5='../processed_data/ERA5_25km_tas_climatology.nc'
f_out=${f_ERA5/.nc/_CNRM-CM6-1.nc}
f_out=${f_out/25km/GCM_grid}
echo $f_out
cdo -L remapbil,$f_grid $f_ERA5 $f_out

f_grid=$path_to_CMIP6_data'/tas_Amon_IPSL-CM6A-LR_historical_r10i1p1f1_gr_185001-201412.nc'
f_ERA5='../processed_data/ERA5_25km_tas_climatology.nc'
f_out=${f_ERA5/.nc/_IPSL-CM6A-LR.nc}
f_out=${f_out/25km/GCM_grid}
echo $f_out
cdo -L remapbil,$f_grid $f_ERA5 $f_out

f_grid=$path_to_CMIP6_data'/tas_Amon_MIROC6_historical_r10i1p1f1_gn_185001-201412.nc'
f_ERA5='../processed_data/ERA5_25km_tas_climatology.nc'
f_out=${f_ERA5/.nc/_MIROC6.nc}
f_out=${f_out/25km/GCM_grid}
echo $f_out
cdo -L remapbil,$f_grid $f_ERA5 $f_out

f_grid=$path_to_CMIP6_data'/tas_Amon_MIROC-ES2L_historical_r10i1p1f2_gn_185001-201412.nc'
f_ERA5='../processed_data/ERA5_25km_tas_climatology.nc'
f_out=${f_ERA5/.nc/_MIROC-ES2L.nc}
f_out=${f_out/25km/GCM_grid}
echo $f_out
cdo -L remapbil,$f_grid $f_ERA5 $f_out

f_grid=$path_to_CMIP6_data'/tas_Amon_NorCPM1_historical_r10i1p1f1_gn_185001-201412.nc'
f_ERA5='../processed_data/ERA5_25km_tas_climatology.nc'
f_out=${f_ERA5/.nc/_NorCPM1.nc}
f_out=${f_out/25km/GCM_grid}
echo $f_out
cdo -L remapbil,$f_grid $f_ERA5 $f_out
