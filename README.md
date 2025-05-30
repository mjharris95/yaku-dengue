# This repository contains data and scripts to run a project estimating the number of additional cases of dengue caused by Cyclone Yaku and attributing extreme precipitation events in northwest Peru to anthropogenic activity. Please view the pre-print [here](https://www.medrxiv.org/content/10.1101/2024.10.23.24309838v1) for more information about this project.

# Part 1: Climate attribution

The folder 1_Climate_Attribution contains all scripts and outputs for this analysis with its own README file.

# Part 2: Cyclone Yaku and dengue

## Data

adm1_cases.csv: weekly cases in Peru, Ecuador, Colombia, Mexico, and Brazil to estimate cases attributable Cyclone Yaku at the adm1 level

AedesR0Out.csv: converts temperature to a temperature-dependent R0 term (from [Mordecai et al 2017](https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0005568))

anomaly_df.csv: March precipitation anomaly during Cyclone Yaku in Peruvian districts.  ERA5 data extracted using Google Earth Engine.

codubi.rds: Population estimates for districts in Peru from 2010 - 2023. These data were provided by CDC Peru and compiled from the following Peruvian governmental agencies: MTC (Ministerio de Transportes y Comunicaciones, Minister of Transport and Communications); INEI (Instituto Nacional de Estadistica e Informatica, National Institute of Statistcs and Information); CENEPRED (Centro Nacional de Estimaci´on Prevenci´on y Reducci´on del Riesgo de Desastres, The National Center for Estimation, Prevention and Reduction of Disaster Risk). Please see Supplemental Table 5 in the pre-print [here](https://www.medrxiv.org/content/10.1101/2024.10.23.24309838v1) for more information.

covar_df.RData: population-weighted averages of covariates for the moderation analysis.

PER_adm3_cases.rds: weekly cases reported in Peruvian districts (adm3). These data were provided by CDC Peru.

Note that three additional files are missing from this repository because they were too large to share on Github. In order to successfully run the analysis script, please download the following three files from [here](https://drive.google.com/drive/folders/1vxb1OHQLQaAVJs5YCt1YAhI3o_tCVLxp?usp=sharing)

march-clim_df.csv: climate in March from 1973 - 2023 in each spatial unit. ERA5 data extracted using Google Earth Engine.

clim-df.csv: climate every day from 2016 - 2023, used for matching and the synthetic control analysis.  ERA5 data extracted using Google Earth Engine.

peru-main.RData: output of the main synthetic control analysis 

## Scripts

analysis.R: script to conduct all analyses in main text and supplement except for climate attribution

data-prep.R: pieces together climate, case, and population data. Cannot be run in full because disaggregated climate files are not provided.

rgee-march.R: uses Google Earth Engine to extract March climate data from 1973 to 2023 (in order to calculate precipitation anomaly). A Google Earth Engine account and access to some private assets stored by others users are required to run this script.

rgee-tomatch.R: uses Google Earth Engine to extract climate data for the entire year from 2016 - 2023. Also extracts 2020 population. Used for matching and synthetic control. A Google Earth Engine account and access to some private assets stored by others users are required to run this script.

supporting-function.R: defines several functions to that are called repeatedly in the analysis script to conduct matching and the synthetic control analysis and plot results.

## Folders

case-prep-scripts: for each country (Peru, Colombia, Ecuador, Mexico, and Brazil), the R scripts used to pre-process and combine case data files. Cannot be run because disaggregated case files are not provided in this repository (but their sources are described in the pre-print).

cases: outputs of case-prep-scripts, the adm1-level .csv files that give weekly cases

maps: shapefiles for adm1-level maps of Mexico, Colombia, Ecuador, and Brazil. The shapefiles with prefix CDC are region-level (adm1) and district-level (adm3) maps of Peru. five_map is a shapefile that combines the maps of all five countries. Shapefiles provided by CDC Peru.

pop: some outputs of rgee-tomatch.R, population of spatial units in different countries. WorldPop estimates extracted using Google Earth Engine. Additional, population density and proportion population in different land use types for vulnerability index and supplemental matching analyses.
