#!/bin/bash

######################################################################################################
## Regrid the bias-corrected GCM anomalies to ERA5 resolution and add the ERA5 climatology back in
## - this script uses Climate Data Operators (Schulzweida, Uwe. 2023) 
## - Schulzweida, Uwe. (2023). CDO User Guide (2.3.0). Zenodo. https://doi.org/10.5281/zenodo.10020800
######################################################################################################

model_list=("ACCESS-ESM1-5" "CanESM5" "CNRM-CM6-1" "IPSL-CM6A-LR" "MIROC6" "MIROC-ES2L" "NorCPM1")

for model in "${model_list[@]}"; do
    FILES="../processed_data/BC_GCM_tas_residuals_"${model}"_r*.nc"
    for f_GCM_grid_residuals in ${FILES[@]}; do
        f_25km_residuals=${f_GCM_grid_residuals/tas_residuals/tas_25km_residuals}
        outfile=${f_GCM_grid_residuals/tas_residuals/tas_25km_downscaled}
        f_ERA5_25km_clim='../processed_data/ERA5_25km_tas_climatology.nc'
        echo $f_GCM_grid_residuals
        echo $f_25km_residuals
        echo $f_ERA5_25km_clim
        echo $outfile
        echo "regridding residuals from GCM grid to 25km ERA5 grid and adding ERA5 25km climatology"
        cdo -L remapbil,$f_ERA5_25km_clim $f_GCM_grid_residuals $f_25km_residuals
        cdo -L add $f_25km_residuals $f_ERA5_25km_clim $outfile
    done
done
