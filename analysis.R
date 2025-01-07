#install_github("mjharris95/fect") #on first run, use this to install modified fect package
library(tidyverse)
library(sf)
library(lubridate)
library(viridis)
library(cowplot)
library(data.table)
library(PanelMatch)
library(readxl)
library(gsynth)
library(ggplotify)
library(magrittr)
library(fect)
library(beepr)
library(xtable)
library(colorspace)
library(psych)
library(berryFunctions)
set.seed(0514) # make sure to always set the seed so results are replicable
source("supporting-functions.R")

#### PLOT BASELINE CLIMATE, PRECIPITATION ANOMALY, AND ANOMALY DISTRIBUTION ####
annual_df <- read.csv("march-clim_df.csv") %>%
  # calculate mean precip in reference period% 
  mutate(date = as_date(date)) %>%
  mutate(year = year(date),
         day = day(date)) %>%
  filter(day >= 7 & day <= 20) %>% # between the 7th and 20th
  group_by(year, id, country) %>%
  summarize(rain = mean(rain),
            temp = mean(temp)) 

# fix ubigeos
annual_df$id[annual_df$country=="PER3"] <- str_pad(annual_df$id[annual_df$country=="PER3"], 6, pad="0")


# select districts with cases in 2023
case_df<-readRDS("PER_adm3_cases.RDS")

# identify districts with cases reported in 2023
cases_2023<-case_df %>%
  filter(year==2023) %>%
  select(ubigeo) %>%
  unlist() %>%
  unique()

# calculate anomaly
anomaly_df <- annual_df %>% #filter(!year %in% c(2017, 2023)) %>% # remove 2017, El Nino Year
  group_by(id, country) %>%
  filter(country == "PER3") %>%
  summarize(mean_rain = mean(rain),
            mean_temp = mean(temp)) %>%
  right_join(annual_df) %>%
  filter(year == 2023) %>%
  mutate(diff_rain = rain - mean_rain) %>% 
  ungroup()

write.csv(anomaly_df, "anomaly_df.csv", row.names=FALSE)

# Plot distribution of preciptiation anomalies across districts
anomaly_df %>% filter(country %in% c("PER3")) %>%
 #mutate(adm = ifelse(country=="PER3", "A. District", "B. Region")) %>%
  ggplot()+
  geom_histogram(aes(x=diff_rain*1000))+
  xlab("Precipitation Anomaly")+
 #facet_wrap(~adm, scale="free_y")+
  theme_classic()

ggsave("figs/anomaly_dist.pdf", height=4, width=4, units="in")

# make maps of baseline climate conditions
map3 <- read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo) %>% 
  merge(anomaly_df %>% filter(country=="PER3"))

map3 %>%  filter(country=="PER3") %>%
  filter(diff_rain > .0085 & id %in% cases_2023) %>%
  summarise(id = "extreme") -> extreme_map3 # treated districts most heavily impacted by extreme precip

# merge data and plot for adm1
p2a <- ggplot() + 
  geom_sf(data=map3, aes(fill=diff_rain*1000)) +
  geom_sf(data = extreme_map3, col = "black", fill=NA, linewidth = .4)+ # bold outline of extreme precip districts
  scale_fill_viridis("Precipitation Anomaly (mm/day)", option="H")+
  theme_void()+
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,
                                barwidth=11))

p2b <- ggplot() + 
  geom_sf(data=map3, aes(fill=mean_rain*1000)) +
  geom_sf(data = extreme_map3, col = "black", fill=NA, linewidth = .4)+ # bold outline of extreme precip districts
  scale_fill_viridis("Historic Precipitation (mm/day)", option="viridis", 
                     direction = -1)+
  theme_void()+
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,
                                barwidth=11))

p2c <- ggplot()+
  geom_sf(data=map3, aes(fill=mean_temp)) +
  geom_sf(data = extreme_map3, col = "black", fill=NA, linewidth = .4)+ # bold outline of extreme precip districts
  scale_fill_viridis("Historic Temperature (C)", option="plasma", breaks=c(5, 10, 15, 20, 25))+
  theme_void()+
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,
                                barwidth=11))

plot_grid(p2a, p2b, p2c, nrow=1, labels="AUTO")
ggsave("figs/climate_map.pdf", height=4, width=8, units="in")
ggsave("figs/climate_map.png", dpi=320, height=4, width=8, units="in")

# plot temperature-dependent R0 curve
read.csv("AedesR0Out.csv") %>%
  ggplot() +
  geom_line(aes(x=temperature, y=aegypti.R0.median))+
  theme_classic()+
  ylab("Relative R0")+
  xlab("Temperature (C)")

ggsave("figs/relr0.pdf", height=4, width=8, units="in")



# read in peru map
per_map <- read_sf("maps/CDC_Departamentos.shp")

# Plot percentile anomaly in March
# read in precipitation
fread("march-clim_df.csv") %>%
  # filter to Peru at admin1
  filter(country=="PER") %>%
  # get year
  mutate(year = year(date)) %>%
  # take March mean
  group_by(id, year) %>%
  summarize(mean_rain = mean(rain)) %>%
  # calculate percentiles
  group_by(id) %>%
  mutate(pctile = percent_rank(mean_rain)) %>%
  filter(year == 2023) %>%
  mutate(pctile_dis = case_when(
    pctile > .95 ~ "> 95th",
    pctile > .90 ~ "> 90th",
    pctile >= .84 ~ ">= 84th",
    pctile < .84 ~ "Non-extreme (< 84th)"
  )) %>%
  rename(DEPARTAMEN = id) %>%
  # append to Peru map 
  merge(per_map, .) %>%
  # plot
  ggplot() + 
  geom_sf(aes(fill=pctile_dis))+
  scale_fill_manual("Percentile",
                    values=c("white", "#ffa590", "#ff4122", "#c61a09"),
                    breaks=c("Non-extreme (< 84th)", ">= 84th", "> 90th", "> 95th"))+
  theme_void()

ggsave("figs/pctile-anomaly.pdf", height=4, width=4, units="in")

#### PREPARE INPUT DATA FOR SYNTHETIC CONTROL ANALYSIS ####

# define coastal districts (for supplemental analysis)
coastal_dept <- c("TUMBES", "PIURA", "LAMBAYEQUE",
                  "CAJAMARCA", "LA LIBERTAD", "ANCASH",
                  "CALLAO", "LIMA",
                  "ICA", "AREQUIPA", "MOQUEGUA", "TACNA")


coastal_dist <- read_xlsx("maps/UBIGEODISTRITOS.XLSX") %>%
  filter(NOMBDEP %in% coastal_dept) %>%
  select(IDDIST) %>%
  mutate(IDDIST = str_pad(IDDIST, 6, pad="0")) %>%
  unlist()

# read in shapefiles
per_map<-read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo)

dept_map <- read_sf("maps/CDC_Departamentos.shp") 

# ignore observations after 2023
case_df <- readRDS("PER_adm3_cases.RDS") %>%
  filter(year <= 2023) %>%
  rename(id = ubigeo)

# identify districts with cases reported in 2023
cases_2023<-case_df %>%
  filter(year==2023) %>%
  select(id) %>%
  unlist() %>%
  unique()

pop_df <- readRDS("codubi.rds") %>%
  rename(pop = pobtot,
         id = ubigeo,
         year = ano)  %>%
  mutate(year = as.numeric(year)) %>% 
  select(pop, id, year)

# handle districts without population reported in a year (assume it was the same as the following year)
for(test_year in 2016:2010){
  pop_df %>% 
    filter(year %in% c(test_year, test_year+1)) %>%
    group_by(id) %>%
    summarize(frq = n()) %>% 
    filter(frq < 2) %>%
    select(id) %>%
    unlist() -> missing
  
  pop_df %>% 
    filter(id %in% missing & year == test_year + 1) %>%
    mutate(year = test_year) %>%
    rbind(pop_df) -> pop_df
}  

pop_df %<>% filter(!is.na(pop))



#### RUN MAIN ANALYSIS: MATCHING AND SYNTHETIC CONTROL, OUTPUT RESULTS  ####
# run main analysis, plot all figures, and save output with "adm3-allper" prefix

match_out_allper <- match_fun(anomaly_upper = .0085, anomaly_lower = .007,
                              cases_2023 = cases_2023,
                              my_country = "PER3", 
                              match_num=10,
                              plot_bal=TRUE, plot_map=TRUE, 
                              file_prefix="adm3-allper",
                              map=per_map, big_map=dept_map)

synth_out_allper <- synth_fun(case_df, match_out_allper, pop_df, "adm3-allper", 
                              att_plot=TRUE, spatt_plot=FALSE, map=per_map, 
                              big_map=dept_map, use_clim=TRUE, use_r0=FALSE,
                              start_year = 2016, inc=TRUE)

# save matching and synthetic control outputs
save(match_out_allper, synth_out_allper, file="peru-main.RData")

# Get names of treated districts
read_xlsx("maps/UBIGEODISTRITOS.XLSX") %>%
       mutate(IDDIST = str_pad(IDDIST, 6, pad="0")) %>%
       filter(IDDIST %in% match_out_allper$treated_names) %>%
       rename(Region = NOMBDEP,
              Province = NOMBPROV,
              District = NOMBDIST) %>% 
       select(Region, Province, District) %>%
       arrange(Region) %>%
       xtable(., type = "latex", row.names=FALSE) %>%
       print(file = "figs/treated-dist-names.tex", include.rownames=FALSE)

# Get table about treated, untreated, and matched control districts
match_out_allper$table

# Examine values for precip effects: print
synth_out_allper$gsynth_obj$est.att %>% 
  tail(10)

# Examine values: construct table
synth_out_allper$gsynth_obj$est.att %>% 
  tail(13) %>%
  data.frame() %>%
  mutate(time = as.numeric(rownames(synth_out_allper$gsynth_obj$est.att) %>% tail(13)),
         ind = row_number()) %>% 
  mutate(start_date = as_date("2023-03-25")+(ind-4)*28) %>%
  mutate(end_date = start_date + 27) %>%
  mutate(start_date  = format(start_date, "%b %d"),
         end_date = format(end_date, "%b %d")) %>%
  mutate(ATT = mid.cases, CI.lower=lower.cases, CI.upper=upper.cases) %>%
  mutate(`Number Attributable Cases` = paste0(round(ATT), " (", round(CI.lower), " - ", round(CI.upper), ")")) %>%
  mutate(`Percent Attributable Cases` = paste0(round(mid.pct, digits=2)*100, " (", 
                                               round(lower.pct, digits=2)*100, ", ",
                                               round(upper.pct, digits=2)*100, ")"
  )) %>%
  mutate(`p-value` = round(recalc.p, digits=3)) %>%
  rename(`Reported Cases` = "obs.cases") %>%
  # mutate(`Number Non-Attributable Cases` = paste0(round(`Reported Cases` - mid.cases), " (", round(`Reported Cases`- lower.cases), " - ", round(`Reported Cases` - upper.cases), ")")) %>%
  mutate(`Dates` = paste(start_date, "-", end_date)) %>%
  select(`Dates`, `Percent Attributable Cases`, `Number Attributable Cases`, `Reported Cases`, `p-value`) %>%
  xtable(., type = "latex", row.names=FALSE, digits=c(0,0,0,0,0,3)) %>%
  print(file = "adm3-allper_table.tex", include.rownames=FALSE)

# Output table for covariate coefficients
synth_out_allper$gsynth_obj$est.beta

# Plot latent factors
synth_out_allper$gsynth_obj$factor %>% 
  data.frame() %>% 
  mutate(time=row_number()) %>% 
  pivot_longer(!time, names_to="Factor", names_prefix="X", values_to="Estimate") %>% 
  ggplot() + 
  geom_line(aes(x=time, y=Estimate, color=Factor, group=Factor))+
  scale_x_continuous(breaks=synth_out_allper$year_ind,
                     labels=synth_out_allper$years)+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Time")

ggsave("figs/lf-vals.pdf", height=4, width=4, units="in")

# plot factor loadings on a map
lp <- list()
for(i in 1:5){
  lp[[i]] <- data.frame("Loading" = synth_out_allper$gsynth_obj$lambda[,i],
                        "id" = synth_out_allper$gsynth_obj$id) %>%
    merge(per_map, .) %>%
    ggplot()+
    geom_sf(aes(fill=Loading), color="grey40", size=.01)+
    geom_sf(data=dept_map, fill=NA, color="black", size=.01)+
    theme_map()+
    ggtitle(paste0("Latent Factor ",i))+
    scale_fill_continuous_diverging(palette = "Purple-Green")
}

plot_grid(lp[[1]], 
          lp[[2]],
          lp[[3]],
          lp[[4]],
          lp[[5]], nrow=3)

ggsave("figs/fl-maps.pdf", height=8, width=8, units="in")


# Output values of attributable cases per thousand and sort by that
dist_name <- coastal_dist <- read_xlsx("maps/UBIGEODISTRITOS.XLSX") %>%
  mutate(IDDIST = str_pad(IDDIST, 6, pad="0")) %>%
  rename(id = IDDIST)

lapply(match_out_allper$treated_names, function(x)
  synth_out_allper$gsynth_obj$group.output[[x]]$att.on %>% tail(9) %>% head(-1) %>% sum() %>% data.frame(att.inc = .*1000, id=x)) %>%
  do.call(rbind, .) %>%
  arrange(desc(att.inc)) %>%
  left_join(dist_name)



#### RUN SUPPLEMENTAL SYNTHETIC CONTROL ANALYSES ####
### Run for just coastal
match_out_coast <- match_fun(anomaly_upper = .0085, anomaly_lower = .007,
                             coastal_dist = coastal_dist, cases_2023 = cases_2023,
                             match_num=10,
                             my_country = "PER3", 
                             plot_bal=FALSE, plot_map=FALSE, file_prefix="adm3-coast",
                             map=per_map, big_map=dept_map)

synth_out_coast <- synth_fun(case_df, match_out_coast, pop_df, "adm3-coast",  
                             att_plot=TRUE, spatt_plot=FALSE, map=per_map, 
                             big_map = dept_map, use_clim=TRUE, use_r0=FALSE,
                             start_year = 2016, inc=TRUE)


### Without climate covariates 
synth_out_allper_noclim <- synth_fun(case_df, match_out_allper, pop_df, "adm3-allper-noclim", 
                                     use_clim=FALSE,
                                     att_plot=TRUE, spatt_plot=FALSE, 
                                     start_year = 2016, inc=TRUE)

synth_out_coast_noclim <- synth_fun(case_df, match_out_coast, pop_df, "adm3-coast-noclim", 
                                    use_clim=FALSE,
                                    att_plot=FALSE, spatt_plot=FALSE,
                                    inc=TRUE,
                                    start_year = 2016)

save(synth_out_coast, synth_out_allper_noclim, synth_out_coast_noclim, file="scenarios.RData")
# compare no clim/with clim x coastal/all districts
i <- 1
rsq_df <- data.frame()
scen_att_df <- data.frame()

# grab number attributable cases and rmse for all models
for(df in list(synth_out_allper$gsynth_obj, synth_out_allper_noclim$gsynth_obj, 
               synth_out_coast$gsynth_obj, synth_out_coast_noclim$gsynth_obj)){
  
  # set names of models
  clim_str <- ifelse(i %% 2 == 0, "\n(- clim.)", "\n(+ clim.)")
  match_str <- ifelse(i <= 2, "All", "Coastal")
  name_str <- paste(match_str, clim_str)
  
  rsq_df %<>% rbind(., data.frame(name = name_str,
                                  rsq = df$rsq))
  
  
  # will plot attributable cases through end of year
  scen_att_df <-  df$est.att %>%
    data.frame() %>%
    tail(10) %>%
    mutate(time = row_number(),
           name= name_str) %>%
    select(mid.cases, obs.cases, lower.cases, upper.cases, name, time) %>%
    rbind(scen_att_df)
  
  i <- i + 1
}

# plot of R2
scenarios_p1 <- rsq_df %>%
  ggplot()+
  geom_bar(aes(x=name, y=rsq, fill=name), stat="identity")+
  theme_classic() + 
  theme(legend.position="none")+
  xlab("")+
  ylab("R2")


# make x-axis for below figure dates
epiweek_df <- data.frame(dates= seq.Date(from = as_date("2023-03-25"),
                                         to = as_date("2023-12-29"),
                                         by = "28 days"),
                         time=1:10) 

# only keep observed cases calculated across all districts (instead of just coastal)
scen_att_df %<>%
  group_by(time) %>%
  summarize(obs.cases = max(obs.cases)) %>%
  right_join(scen_att_df %>% select(-obs.cases))

# plot number of attributable cases over time

scenarios_p2 <- scen_att_df %>%
  filter(time <= 10) %>%
  left_join(epiweek_df) %>%
  ggplot() +
  geom_pointrange(aes(x=dates, y=mid.cases, ymin=lower.cases, ymax=upper.cases, color=name), 
                  size=.2, position=position_dodge(width=10))+ #95% confidence interval for each model
  geom_line(aes(y=obs.cases, x=dates))+
  scale_color_discrete("")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+ #make nice x-axis
  theme_classic()+
  geom_hline(yintercept=0, linetype="dashed")+
  theme(legend.position="bottom")+
  ylab("Attributable cases")+
  xlab("Time")

# combine plots and save
plot_grid(scenarios_p1, scenarios_p2+theme(legend.position="none"), rel_widths=c(1,2), labels="AUTO") %>%
  plot_grid(., get_legend(scenarios_p2), rel_heights=c(9,1), nrow=2)
ggsave("figs/comp-scenarios.pdf", height=4, width=8)

# Test effects of including relative R0 instead of mean temperature in model
synth_out_r0 <- synth_fun(case_df, match_out_allper, pop_df, "adm3-allper-r0", 
                          use_clim=TRUE, use_r0 = TRUE,
                          att_plot=TRUE, spatt_plot=FALSE, map=per_map,
                          start_year = 2016, inc=TRUE)

## Test effects of excluding observations prior to 2016 (by not setting start_year)
synth_out_allper_pre16 <- synth_fun(case_df, match_out_allper, pop_df, "adm3-allper-pre16", 
                                    use_clim=TRUE,
                                    use_r0=FALSE,
                                    att_plot=TRUE,
                                    inc=TRUE)

# Test effects of excluding heights of COVID-19 pandemic (2020 - 2021)
synth_out_allper_nocovid <- synth_fun(case_df, match_out_allper, pop_df, "adm3-nocovid", 
                                      att_plot=FALSE, spatt_plot=FALSE, 
                                      use_clim=TRUE, 
                                      use_r0=FALSE,
                                      exclude_year = c(2020, 2021), inc=TRUE)

# manually plot so that the excluded years are properly accounted for 
synth_out_allper_nocovid$gsynth_obj$est.att <- rbind(synth_out_allper_nocovid$gsynth_obj$est.att[1:130,],
                                                     synth_out_allper_nocovid$gsynth_obj$est.att[1:26,]*NA,
                                                     synth_out_allper_nocovid$gsynth_obj$est.att[131:156,])

synth_out_allper_nocovid$gsynth_obj$Y.dat <- rbind(synth_out_allper_nocovid$gsynth_obj$Y.dat[1:130,],
                                                   synth_out_allper_nocovid$gsynth_obj$Y.dat[1:26,]*NA,
                                                   synth_out_allper_nocovid$gsynth_obj$Y.dat[131:156,])

synth_out_allper_nocovid$gsynth_obj$Y.ct <- rbind(synth_out_allper_nocovid$gsynth_obj$Y.ct[1:130,],
                                                  synth_out_allper_nocovid$gsynth_obj$Y.ct[1:26,]*NA,
                                                  synth_out_allper_nocovid$gsynth_obj$Y.ct[131:156,])

# extract population weights
act.locs <- synth_out_allper_nocovid$gsynth_obj $id[synth_out_allper_nocovid$gsynth_obj$tr]
tr_pop <- pop_df %>% filter(year==2023 & id %in% act.locs) %>% select(pop, id) %>% distinct() %>% filter(!is.na(pop)) %>% select(pop) %>% sum()
act.pops <- sapply(act.locs, function(loc) pop_df$pop[which(pop_df$id == loc & pop_df$year == 2023)])
act.weights <- unlist(act.pops)/tr_pop

att_plot(case_df, synth_out_allper_nocovid$gsynth_obj,
         synth_out_allper_nocovid$cyclone_step+26,
         tr_pop, act.weights,
         "adm3-nocovid",
         # set year indices and years for x-axis
         c(synth_out_allper_nocovid$year_ind, tail(synth_out_allper_nocovid$year_ind,1)+13, tail(synth_out_allper_nocovid$year_ind,1)+26),
         sort(c(synth_out_allper_nocovid$years, 2020, 2021)),
         inc=TRUE)

## Test effects of varying anomaly threshold for control vs treated
# initialize variables to store values
match_anomvar <- list()
anomvar_df <- data.frame()

for(anomaly_upper in c(.007, .0085, .01)){
  
  for(anomaly_lower in c(.0055,.007,.0085,.01)){
    if(anomaly_lower <= anomaly_upper){
      # fit synthetic control model for given anomaly thresholds
      my_match <- match_fun(anomaly_upper = anomaly_upper, 
                            anomaly_lower = anomaly_lower,
                            cases_2023 = cases_2023,
                            my_country = "PER3",
                            match_num=10)
      
      my_synth <- synth_fun(case_df, 
                            my_match, 
                            pop_df, 
                            NA,
                            use_clim=TRUE,
                            use_r0=FALSE,
                            inc=TRUE,
                            start_year = 2016)
      
      # store desired values for plotting
      this_df <- data.frame(anomaly_lower = toString(anomaly_lower*1000),
                            anomaly_upper = toString(anomaly_upper*1000),
                            nmatched = my_match$match_names %>% length(),
                            ntreated = my_match$treated_names %>% length(),
                            temp_bal = my_match$balance[["mean_temp"]],
                            rain_bal = my_match$balance[["mean_rain"]],
                            rmse = my_synth$gsynth_obj$rmse,
                            rsq = my_synth$gsynth_obj$rsq,
                            att.est = my_synth$att_nums$mid_pct,
                            att.lower = my_synth$att_nums$lower_pct,
                            att.upper = my_synth$att_nums$upper_pct,
                            num.cases = my_synth$att_nums$mid_cases)
      
      anomvar_df %<>% rbind(this_df)

      # save memory by deleting the match/synth objects once info is extracted
      rm(my_match)
      rm(my_synth)
      
      # print model specs to keep track of what has run
      print(paste("Upper:", anomaly_upper, ". Lower:", anomaly_lower))
    }
  }
}

# reorder x-axis
anomvar_df$anomaly_lower <- factor(anomvar_df$anomaly_lower, levels=c("5.5", "7", "8.5", "10"))
anomvar_df$anomaly_upper <- factor(anomvar_df$anomaly_upper, levels=c("7", "8.5", "10"))

# these will go into the balance plot: p11 = Abs Mean Stand Diff for Temp, 
# p12 = Abs Mean Stand Diff for Precip, p13 = Rsq
anomvar_df %<>% distinct()
anom_p11 <- anomvar_df %>%
  ggplot() + 
  geom_bar(aes(x=anomaly_upper, y=abs(temp_bal), fill=anomaly_lower), stat="identity", position="dodge")+
  ggtitle("Balance (Temp.)")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  scale_fill_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  ylab("Abs. Mean Stand. Diff.")+
  xlab("")

anom_p12 <- anomvar_df %>%
  ggplot() + 
  geom_bar(aes(x=anomaly_upper, y=abs(rain_bal), fill=anomaly_lower), stat="identity", position="dodge")+
  ggtitle("Balance (Precip.)")+
  xlab("")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  scale_fill_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  ylab("Abs. Mean Stand. Diff.")

anom_p13 <- anomvar_df %>%
  ggplot() + 
  geom_bar(aes(x=anomaly_upper, y=rsq, fill=anomaly_lower), stat="identity", position="dodge")+
  ggtitle("R2 (Cases)")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  scale_fill_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  ylab("R2")+
  xlab("")

anom_p1 <- plot_grid(anom_p11, anom_p12, anom_p13, nrow=1) %>%
  add_sub(label="Precip. Anomaly (Control Threshold)", size=12) %>%
  ggdraw()

# plot of percent attributable cases depending on threshold, with 95% CI
anom_p2 <-  anomvar_df %>%
  ggplot(aes(x=anomaly_upper, color=anomaly_lower)) + 
  geom_pointrange(aes(y=att.est*100, ymin=att.lower*100, ymax=att.upper*100),
                  stat="identity", position = position_dodge2(width = .2))+
  xlab("Precip. Anomaly (Extreme Threshold)")+
  ylab("Attributable \nCases (%)")+
  theme_classic()+
  scale_color_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  theme(legend.position="bottom", text=element_text(size=12))+
  geom_hline(yintercept=c(0, 100), color="grey", linetype="dotted")


# plots of sample sizes: p31 = number of control districts, p32 = number of treated districts
anom_p31 <- anomvar_df %>%
  ggplot() + 
  geom_bar(aes(x=anomaly_upper, y=nmatched, fill=anomaly_lower), stat="identity", position="dodge")+
  ggtitle("Control")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  scale_fill_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  ylab("n")+
  xlab("")+
  scale_y_continuous(limits=c(0, max(anomvar_df$nmatched)))

anom_p32 <- anomvar_df %>%
  ggplot() + 
  geom_bar(aes(x=anomaly_upper, y=ntreated, fill=anomaly_lower), stat="identity", position="dodge")+
  ggtitle("Extreme")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  scale_fill_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  ylab("n")+
  xlab("")+
  scale_y_continuous(limits=c(0, max(anomvar_df$nmatched)))

anom_p3 <- plot_grid(anom_p31, anom_p32, nrow=1) %>%
  add_sub(label="Precip. Anomaly (Control Threshold)", size=12) %>%
  ggdraw()

# generate full plot and save
plot_grid(plot_grid(anom_p1, anom_p3, ncol=2, rel_widths=c(3,2), labels="AUTO"),
          anom_p2,
          nrow=2, 
          labels=c("", "C"))


ggsave(file="figs/anomaly-variation.png", height=6, width=10, units="in")

### Test out varying the number of control units to which each treated is matched (match number, mn)
# initialize dataframe and list to store values
mnvar_df <- data.frame()
mnvar_att <- list()

for(i in 1:3){
  match_num <- c(5, 10, 15)[i]
  
  # for each value of match_num, rerun matching and synthetic control 
  match_mn <- match_fun(anomaly_upper = .0085, anomaly_lower = .007,
                        cases_2023 = cases_2023,
                        my_country = "PER3",
                        file_prefix= NA,
                        match_num=match_num)
  
  synth_mn <- synth_fun(case_df, match_mn, pop_df, NA, 
                        use_clim=TRUE, use_r0=FALSE,
                        start_year = 2016, inc=TRUE)
  
  # store values
  mnvar_att[[i]] <- synth_mn$gsynth_out$est.att
  
  mnvar_df %<>% rbind(data.frame(match_num = match_num,
                                 nmatched = match_mn$match_names %>% length(),
                                 ntreated = match_mn$treated_names %>% length(),
                                 temp_bal = match_mn$balance[["mean_temp"]],
                                 rain_bal = match_mn$balance[["mean_rain"]],
                                 rmse = synth_mn$gsynth_obj$rmse,
                                 rsq = synth_mn$gsynth_obj$rsq,
                                 att.est = synth_mn$att_nums$mid_pct,
                                 att.lower = synth_mn$att_nums$lower_pct,
                                 att.upper = synth_mn$att_nums$upper_pct,
                                 num.cases = synth_mn$att_nums$mid_cases))
  
  if(i == 3){
    # additional run for no matching
    i <- 4
    match_num <- "All"
    # edit the match_out dataframe to include everything in the control pool
    match_mn$match_names <- filter(match_mn$df, is_control %in% c("Non-extreme Precipitation", "Matched Control")) %>%
      select(id) %>%
      unique() %>% 
      unlist()
    
    # conduct synthetic control analysis 
    synth_mn <- synth_fun(case_df, match_mn, pop_df, NA, 
                          use_clim=TRUE, use_r0=FALSE,
                          start_year = 2016, inc=TRUE)
    # store values
    mnvar_att[[i]] <- synth_mn$gsynth_out$est.att
    
    mnvar_df %<>% rbind(data.frame(match_num = "All",
                                   nmatched = match_mn$match_names %>% length(),
                                   ntreated = match_mn$treated_names %>% length(),
                                   temp_bal = match_mn$balance[["mean_temp"]],
                                   rain_bal = match_mn$balance[["mean_rain"]],
                                   rmse = synth_mn$gsynth_obj$rmse,
                                   rsq = synth_mn$gsynth_obj$rsq,
                                   att.est = synth_mn$att_nums$mid_pct,
                                   att.lower = synth_mn$att_nums$lower_pct,
                                   att.upper = synth_mn$att_nums$upper_pct,
                                   num.cases = synth_mn$att_nums$mid_cases))
    
  }
}

# maintain correct order on x-axis
mnvar_df$match_num <- factor(mnvar_df$match_num, levels=c("5", "10", "15", "All"))

# in case anything was run twice by accident
mnvar_df %<>% distinct()

# these will go into the balance plot: p11 = Abs Mean Stand Diff for Temp, 
# p12 = Abs Mean Stand Diff for Precip, p13 = Rsq
mn_p11 <- mnvar_df %>%
  ggplot() + 
  geom_bar(aes(x=match_num, y=abs(temp_bal)), stat="identity")+
  ggtitle("Balance (Temp.)")+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  ylab("Abs. Mean Stand. Diff.")+
  xlab("")

mn_p12 <- mnvar_df %>%
  ggplot() + 
  geom_bar(aes(x=match_num, y=abs(rain_bal)), stat="identity")+
  ggtitle("Balance (Precip.)")+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  ylab("Abs. Mean Stand. Diff.")+
  xlab("")

mn_p13 <- mnvar_df %>%
  ggplot() + 
  geom_bar(aes(x=match_num, y=rsq), stat="identity")+
  ggtitle("R2 (Cases)")+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  ylab("R2")+
  xlab("")

mn_p1 <- plot_grid(mn_p11, mn_p12, mn_p13, nrow=1) %>%
  add_sub(label="Matching Number", size=12) %>%
  ggdraw()

# plot of percent attributable cases depending on threshold, with 95% CI
mn_p2 <-  mnvar_df %>%
  ggplot(aes(x=as.factor(match_num))) + 
  geom_pointrange(aes(y=att.est*100, ymin=att.lower*100, ymax=att.upper*100),
                  stat="identity")+
  xlab("Matching Number")+
  ylab("Attributable \nCases (%)")+
  theme_classic()+
  theme(legend.position="bottom")+
  geom_hline(yintercept=c(0, 100), color="grey", linetype="dotted")+
  scale_x_discrete(breaks=c("5", "10", "15", "All"))

# plots of sample sizes: p31 = number of control districts, p32 = number of treated districts
mn_p31 <- mnvar_df %>%
  ggplot() + 
  geom_bar(aes(x=match_num, y=nmatched), stat="identity")+
  ggtitle("Control")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  ylab("n")+
  xlab("")+
  scale_y_continuous(limits=c(0, max(mnvar_df$nmatched)))

mn_p32 <- mnvar_df %>%
  ggplot() + 
  geom_bar(aes(x=match_num, y=ntreated), stat="identity")+
  ggtitle("Extreme Precipitation")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  ylab("n")+
  xlab("")+
  scale_y_continuous(limits=c(0, max(mnvar_df$nmatched)))

mn_p3 <- plot_grid(mn_p31, mn_p32, nrow=1) %>%
  add_sub(label="Matching Number", size=12) %>%
  ggdraw()


# combine plots and save
plot_grid(plot_grid(mn_p1, mn_p3, ncol=2, rel_widths=c(3,2), labels="AUTO"),
          mn_p2,
          nrow=2, 
          labels=c("", "C"))

ggsave(file="figs/matchnum-variation.png", height=6, width=10, units="in")

### Test out varying the number of latent factors
# store values here
synth_lf <- list()
synth_lf_df <- data.frame()

for(lf_num in 0:5){
  # loop through number of latent factors (lf_num), using set of treated/control districts for main model
  this_synth <- synth_fun(case_df, match_out_allper, pop_df, "adm3-lfvar",
                          use_clim=TRUE, 
                          use_r0=FALSE, 
                          lf_num=lf_num, start_year = 2016, 
                          inc=TRUE)
  
  # store outputs
  synth_lf[[lf_num+1]] <- this_synth
  
  synth_lf_df %<>% rbind(data.frame(lf_num = lf_num,
                                    rsq = this_synth$gsynth_obj$rsq,
                                    att.est = this_synth$att_nums$mid_pct,
                                    att.lower = this_synth$att_nums$lower_pct,
                                    att.upper = this_synth$att_nums$upper_pct,
                                    num.cases = this_synth$att_nums$mid_cases))
  
  # keep track of where we are by printing
  print(lf_num)
  rm(this_synth)
}

# get cross-validation results
synth_lf_df$MSPE <- synth_out_allper$gsynth_obj$CV.out.ife[,"MSPE"]
synth_lf_df$IC <- synth_out_allper$gsynth_obj$CV.out.ife[,"IC"]
synth_lf_df$PC <- synth_out_allper$gsynth_obj$CV.out.ife[,"PC"]

# in case it ran twice, only show distinct entries
synth_lf_df %<>% distinct()

# plot the different balance metrics provided by the fect package as facets
lf_p1a <- synth_lf_df %>% 
  ggplot() + 
  geom_bar(aes(x=lf_num, y=IC), stat="identity")+
  xlab("Number of Latent Factors")+
  theme_classic()+
  theme(legend.position="none")+
  scale_x_continuous(breaks=0:5)

lf_p1b <- synth_lf_df %>% 
  ggplot() + 
  geom_bar(aes(x=lf_num, y=MSPE), stat="identity")+
  xlab("Number of Latent Factors")+
  theme_classic()+
  theme(legend.position="none")+
  scale_x_continuous(breaks=0:5)

lf_p1c <- synth_lf_df %>% 
  ggplot() + 
  geom_bar(aes(x=lf_num, y=PC), stat="identity")+
  xlab("Number of Latent Factors")+
  theme_classic()+
  theme(legend.position="none")+
  scale_x_continuous(breaks=0:5)

lf_p1 <- plot_grid(lf_p1a, lf_p1b, lf_p1c, ncol=3) 


# plot percent attributable cases vs number of latent factors with 95% confidence intervals
lf_p2 <- ggplot(synth_lf_df, aes(x=lf_num)) + 
  geom_pointrange(aes(y=att.est*100, ymin=att.lower*100, ymax=att.upper*100),
                  stat="identity", position = position_dodge2(width = .2))+
  xlab("Number of Latent Factors")+
  ylab("Attributable \nCases (%)")+
  geom_hline(yintercept=0, color="grey", linetype="dashed")+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks=0:5)

# combine plots, save
plot_grid(lf_p1, lf_p2, nrow=2, labels="AUTO")
ggsave("figs/lf-variation.pdf", height=8, width=8, units="in")

## Analysis at admin1 (regional) level
# transform names to be compatible with case data
per_rename <- read_xlsx("maps/UBIGEODISTRITOS.XLSX") %>%
  mutate(id = paste0("PE", substr(IDDIST,1,2))) %>%
  rename(name=NOMBDEP) %>%
  select(name, id) %>%
  distinct()

# read in case data,
# filter to the last week when there's data reported for all countries
case_df_adm1<-read.csv("D:/Attribution/adm1_cases.csv") %>%
  left_join(per_rename) %>%
  mutate(id = ifelse(!is.na(name), name, id)) %>%
  select(-name) %>%
  filter(year < 2024) %>%
  filter(year < 2023 | week <= 28) %>%
  distinct()

# which regions reported cases in 2023? 
cases_2023_adm1<-case_df_adm1 %>%
  filter(year==2023) %>%
  select(id) %>%
  unlist() %>%
  unique()

# read in shapefiles
load("maps/five_map")

# identify country borders
country_bounds <- five_map %>%
  group_by(country) %>%
  summarize(geometry=st_union(geometry))

# Run analysis
match_out_adm1 <- match_fun(anomaly_upper = .007, anomaly_lower = NA,
                            cases_2023 = cases_2023_adm1,
                            match_num=10,
                            my_country = c("PER", "COL1", "ECU1", "MEX", "BRA1"), 
                            plot_bal=FALSE, plot_map=FALSE, file_prefix="adm1",
                            map=five_map, big_map=country_bounds,
                            treated_names = extreme_adm1)

pop_df_adm1 <- expand_grid(id=c(match_out_adm1$match_names, match_out_adm1$treated_names),
                           year = 2018:2023)  %>% 
  left_join(., (case_df_adm1 %>% select(id, pop) %>% distinct()))

synth_out_adm1 <- synth_fun(case_df_adm1, match_out_adm1, 
                            pop_df_adm1, "adm1",  
                            att_plot=TRUE, spatt_plot=FALSE,
                            use_clim=TRUE, use_r0=FALSE,
                            start_year = 2018, end_week=28, inc=TRUE)


#### ANALYSIS FOR PART 2: SOCIOVULNERABILITY INDICES BEGINS HERE
# would recommend detaching other packages due to interference between dplyr and terra select
library(raster)
library(terra)
library(psych)

# read in shapefile of districts
per_map<-read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo)

# the cov directory contains .tif files of different vulnerability indices - excluded from
# public respository for privacy reasons
files <- dir("cov")

#exclude population density
files <- files[!startsWith(files, "r_dpob")]

# loop through covariate files
for(file in files[startsWith(files, "r_")]){
  
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
  dept <- ifelse(dept == "MARTIN", "SAN MARTIN", dept)
  
  # read in corresponding population density file
  dpob <-  switch(dept,
                  "TUMBES" = "r_dpob_TUMBES_050.tif",
                  "LA LIBERTAD" = "r_dpob_LA_LIBERTAD_050.tif",
                  "LAMBAYEQUE" = "r_dpob_LAMBAYEQUE_050.tif",
                  "PIURA" = "r_dpob_PIURA_050.tif",
                  "CAJAMARCA" = "r_dpob_CAJAMARCA_050.tif",
                  "SAN MARTIN" = "r_dpob_SAN_MARTIN_050.tif" ) %>%
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
  
  # weight covariates based on population
  socio_mult <- socio * dpob
  
  # get population-weighted sum
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
  
  
  # this loop takes a while to run, save each step
  save(covar_df, file="covar_df.RData")
}


load("covar_df.RData")

# load in the matching and gsynth results
load("peru-main.RData")

# filter covariates to the extreme precip districts
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

# determine number of factors to use based on vss
covar_df %>% 
  dplyr::select(-c(id, socio_vtp)) %>%
  vss(n=8, rotate="varimax", fm="mle")

# run pca, store results
covar_df %>% 
  dplyr::select(-c(id, socio_vtp)) %>%
  principal(nfactors=4, rotate="varimax", scores=TRUE) -> pca_res

# extract labels for main variables in each rotated components,
# use this to understand rotated components & helper for plotting
RC_labs <- pca_res$loadings %>% 
  unclass %>% 
  as.data.frame %>% 
  rownames_to_column() %>% 
  pivot_longer(starts_with("RC")) %>% 
  filter(abs(value) > 0.7) %>% 
  # make this into the correct variable names
  mutate(lab = recode(rowname, 
                      "rain" = "Precipitation", 
                      "vui" = "Flood susceptibility",
                      "socio_vpr" = "Low-quality walls",
                      "socio_vtc" = "Low-quality roofs",
                      "temp" = "Temperature",
                      "socio_vps" = "Low-quality floors",
                      "socio_vtp" = "Low-quality housing",
                      "socio_acc" = "Nonpublic \nwater source")) %>%
  mutate(lab = paste0( #ifelse(value < 0, "- ", "+ "), 
    lab, " (",  round(value, 2), ")")) %>% 
  group_by(name) %>% 
  arrange(name, desc(abs(value))) %>% 
  summarise(lab = paste(lab, collapse = "\n")) %>% 
  mutate(xval = as.numeric(gsub("RC", "", name)))


# add RC values to dataframe
pca_res$scores %>%
  data.frame() %>%
  cbind(covar_df, .) -> covar_df

# extract attributable incidence in each district across the post-cyclone period (April 22 - Nov 3)
eff_ind <- which(rownames(synth_out_allper$gsynth_obj$est.att) %in% c("3", "4", "5", "6", "7", "8", "9"))

sapply(covar_df$id, function(dist_id) synth_out_allper$gsynth_obj$eff[eff_ind, which(synth_out_allper$gsynth_obj$id==dist_id)] %>% sum()) %>%
  data.frame(ATT = .,
             id = names(.)) %>%
  right_join(covar_df) -> covar_df

# fit regression 
mod.res <- lm(ATT~RC1+RC2+RC3+RC4, data=covar_df)
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
boot_sample <-  boot_vals %>%
  group_by(id) %>%
  slice_sample(n=609) %>%
  ungroup()


#### IMPORTANT: THESE NEED TO BE UPDATED
# for each bootstrap, refit the model and store the coefficients 
# here, we're resampling across both extreme precip districts and their bootstrapped estimates
suppressWarnings(boot_mod <- lapply(1:1000, function(i) slice_sample(boot_sample, n=55, replace=TRUE) %>%
                                      left_join(., covar_df %>% dplyr::select(-ATT), by="id") %>%
                                      lm(ATT~RC1+RC2+RC3+RC4, data=.)  %>% 
                                      summary() %>% 
                                      coef() %>% 
                                      data.frame() %>%
                                      dplyr::select(Estimate) %>%
                                      t() %>%
                                      data.frame()) %>%
                   do.call(rbind, .) %>%
                   #determine the 95% confidence interval and p-value across estimates
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

RC_labs$y <- c(20, 10, 20, 10)

# plot the relationship between the RCs and attributable incidence
p1 <- boot_mod %>%
  filter(var!="Intercept") %>%
  ggplot() + 
  geom_point(aes(x=var, y=mid*1000), size=4)+
  geom_errorbar(aes(x=var, ymin=lower.CI*1000, ymax=upper.CI*1000), width=.1)+
  geom_hline(linetype="dashed", yintercept=0, color="maroon")+
  geom_label(data = RC_labs, 
             aes(x = name, label = lab, y = y), size = 2)+
  theme_classic()+
  scale_y_continuous(limits=c(-10, 25))+
  ylab("Attributable \ncases per thousand")+
  xlab("Factor")+
  theme(text=element_text(size=10))

# plot the temperature vs attributable cases
p2 <- boot_vals %>% 
  group_by(id) %>%
  summarize(min.eff = quantile(ATT, .025),
            max.eff = quantile(ATT, .975)) %>%
  right_join(covar_df) %>%
  ggplot() + 
  geom_linerange(aes(x=temp,ymin=min.eff*1000, ymax=max.eff*1000), 
                 color="gray", alpha=.6, linewidth=.3)+
  geom_point(aes(x=temp, y=ATT*1000),
             size=1, alpha=.6)+
  theme_classic()+
  geom_hline(yintercept=0, linetype="dashed", color="maroon") +
  geom_vline(xintercept=24, linetype="dashed", color="maroon") +
  xlab("Mean Temperature During Yaku (C)")+
  ylab("Attributable \ncases per thousand")+
  theme(text=element_text(size=10))

plot_grid(p1, p2, nrow=2, labels="AUTO")
ggsave(paste0("figs/vul-ind.pdf"), height=11, width=11, units="cm")

