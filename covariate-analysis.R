library(raster)
library(terra)
library(psych)
library(tidyverse)

per_map<-read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo)


covar_df <- data.frame()

files <- dir("cov")

# loop through covariate files
for(file in tail(files[startsWith(files, "r_")],-5)){
  
  # identify the department based on file name
  dept <-  file %>% 
    str_split(., pattern=".tif") %>% 
    unlist() %>% 
    head(1) %>%
    str_split(., pattern="_") %>%
    unlist() %>%
    tail(1) 
  
  # issue with string splitting because LA LIBERTAD is two words
  dept <- ifelse(dept == "LIBERTAD", "LA LIBERTAD", dept)
  
  # read in corresponding population density file
  dpob <-  switch(dept,
                  "TUMBES" = "r_dpob_TUMBES_050.tif",
                  "LA LIBERTAD" = "r_dpob_LA_LIBERTAD_050.tif",
                  "LAMBAYEQUE" = "r_dpob_LAMBAYEQUE_050.tif",
                  "PIURA" = "r_dpob_PIURA_050.tif") %>%
    paste0("cov/", .) %>%
    rast()
  
  # what is the variable name?
  var_name <- file %>% 
    str_split(., "r_") %>%
    unlist() %>%
    tail(-1) %>%
    paste("collapse" = "r_") %>%
    str_split(., "_[:upper:]")  %>%
    unlist() %>%
    head(1) 
  
  # read in the covariate raster
  socio <- rast(paste0("cov/" , file))
  
  # align the rasters
  dpob <- project(dpob, per_map)
  socio <- project(socio, dpob)
  dpob <- crop(dpob, socio)
  
  # weight covarariates based on population
  socio_mult <- socio * dpob
  
  # get total
  socio_sum <- terra::extract(socio_mult, per_map,
                              fun="sum", na.rm=TRUE,
                              bind = TRUE) %>%
    st_drop_geometry() %>%
    data.frame() %>%
    rename(var = 5)
  
  # get population and calculate pop-weighted mean
  covar_df <- terra::extract(dpob, per_map,
                             fun="sum", na.rm=TRUE,
                             bind = TRUE) %>% 
    st_drop_geometry %>%
    data.frame() %>%
    rename(pop = 5) %>%
    left_join(socio_sum) %>%
    mutate(avg_socio = var/pop) %>%
    mutate(var_name = var_name) %>%
    filter(DEPARTAMEN == dept) %>%
    rbind(covar_df)
  
  print(file)
  
  # this loop takes a while to run, save each step
  save(covar_df, file="covar_df-preprocess.RData")
}


load("covar_df-preprocess.RData")

# load in the matching and gsynth results
load("peru-main.RData")

# filter covariates to the cyclone-affected districts
covar_df %<>%
  filter(id %in% match_out_allper$treated_names)

set.seed(0514)

# pivot wider - format for pca package
covar_df %<>%
  distinct() %>%
  dplyr::select(c(id, avg_socio, var_name)) %>%
  pivot_wider(names_from=var_name, values_from=avg_socio) %>%
  distinct()


# append weather information during Cyclone Yaku
covar_df <- read.csv("anomaly_df.csv") %>%
  dplyr::select(id, temp, rain) %>%
  right_join(covar_df) %>%
  filter(!is.nan(vui))

# deetrmine number of factors to use based on vss
covar_df %>% 
  dplyr::select(-c(id)) %>%
  vss(n=8, rotate="varimax", fm="mle")

# run pca, store results
covar_df %>% 
  dplyr::select(-c(id)) %>%
  principal(nfactors=2, rotate="varimax", scores=TRUE) -> pca_res

# extract labels for main variables in each rotated components,
# use this to understand rotated components & helper for plotting
RC_labs <- pca_res$loadings %>% 
  unclass %>% 
  as.data.frame %>% 
  rownames_to_column() %>% 
  pivot_longer(starts_with("RC")) %>% 
  filter(abs(value) > 0.7) %>% 
  # clean up variable names
  mutate(lab = recode(rowname, 
                      "rain" = "Precipitation", 
                      "vui" = "Flood Susceptibility",
                      "socio_vpr" = "Low-quality Walls",
                      "socio_vtc" = "Low-quality Floors")) %>%
  mutate(lab = paste0(ifelse(value < 0, "- ", "+ "), 
                      lab, " (",  round(value, 2), ")")) %>% 
  group_by(name) %>% 
  arrange(name, desc(abs(value))) %>% 
  summarise(lab = paste(lab, collapse = "\n")) %>% 
  mutate(xval = as.numeric(gsub("RC", "", name)))

# add RC values to dataframe
pca_res$scores %>%
  data.frame() %>%
  cbind(covar_df, .) -> covar_df

# extract cyclone-attributable incidence in each district across the post-cyclone period (April 22 - Nov 3)
eff_ind <- which(rownames(synth_out_allper$gsynth_obj$est.att) %in% c("3", "4", "5", "6", "7", "8", "9"))

sapply(covar_df$id, function(dist_id) synth_out_allper$gsynth_obj$eff[eff_ind, which(synth_out_allper$gsynth_obj$id==dist_id)] %>% sum()) %>%
  data.frame(ATT = .,
             id = names(.)) %>%
  right_join(covar_df) -> covar_df

# fit regression 
mod.res <- lm(ATT~RC1+RC2, data=covar_df)
mod <- mod.res$coefficients

# get the bootstrapped attributable incidence estimates from the generalized synthetic control
boot_vals <- lapply(covar_df$id, function(id)
  synth_out_allper$gsynth_obj$est.group.output[[id]]$att.on.boot[eff_ind,] %>%
    colSums() %>%
    data.frame("id" = id,
               "ATT" = .,
               "boot" = 1:1000)) %>%
  do.call(rbind, .) %>%
  filter(!is.na(ATT)) 

# because of how the bootstrapping works, some of the districts were randomly resampled more than others
# we randomly sample the same number of bootstrapped effect estimates for each district to ensure
# our regression isn't biased toward a particular district
boot_sample <-  group_by(id) %>%
  slice_sample(n=609) %>%
  ungroup()

# for each bootstrap, refit the model and store the coefficients 
# here, we're resampling across both cyclone-affected districts and their bootstrapped estimates
suppressWarnings(boot_mod <- lapply(1:1000, function(i) slice_sample(boot_sample, n=45, replace=TRUE) %>% 
                                      left_join(., covar_df %>% dplyr::select(-ATT), by="id") %>%
                                      lm(ATT~RC1+RC2, data=.)  %>% 
                                      summary() %>% 
                                      coef() %>% 
                                      data.frame() %>%
                                      dplyr::select(Estimate) %>%
                                      t() %>%
                                      data.frame()) %>%
                   do.call(rbind, .) %>%
                   # determine the 95% confidence interval and p-value across bootstraps
                   sapply(function(x) c(quantile(x, c(.025, .975)), sum(x<=0)/1000)) %>%
                   t() %>%
                   data.frame() %>%
                   rename("lower.CI"=1,
                          "upper.CI"=2,
                          "p"=3) %>%
                   cbind(., var = rownames(.),
                         mid = mod)) 

# rename y-intercept variable (which confusingly get called x-intercept because of how R default names columns)
boot_mod$var <-  ifelse(boot_mod$var == "X.Intercept.", "Intercept", boot_mod$var)


# plot the relationship between the RCs and cyclone-attributable incidence
p1 <- boot_mod %>%
  filter(var!="Intercept") %>%
  ggplot() + 
  geom_point(aes(x=var, y=mid*1000), size=4)+
  geom_errorbar(aes(x=var, ymin=lower.CI*1000, ymax=upper.CI*1000), width=.1)+
  geom_hline(linetype="dashed", yintercept=0)+
  geom_label(data = RC_labs, 
             aes(x = name, label = lab), y = 20, size = 2.5, fontface = 2)+
  theme_classic()+
  scale_y_continuous(limits=c(-10, 25))+
  ylab("Cyclone-attributable cases per thousand")+
  xlab(" ")

# plot the temperature vs cyclone-attributable cases
p2 <- boot_vals %>% 
  group_by(id) %>%
  summarize(min.eff = quantile(ATT, .025),
            max.eff = quantile(ATT, .975)) %>%
  right_join(covar_df) %>%
  ggplot() + 
  geom_pointrange(aes(x=temp, y=ATT*1000, ymin=min.eff*1000, ymax=max.eff*1000), size=.3, alpha=.6)+
  theme_classic()+
  # visual guides: null effect as horizontal line and potential thermal threshold as vertical line
  geom_hline(yintercept=0, linetype="dashed", color="maroon") +
  geom_vline(xintercept=24, linetype="dashed", color="maroon") +
  xlab("Mean Temperature During Yaku (C)")+
  ylab("Cyclone-attributable cases per thousand")


plot_grid(p1, p2, nrow=1, labels="AUTO")
ggsave(paste0("figs/vul-ind.pdf"), height=4, width=8, units="in")
