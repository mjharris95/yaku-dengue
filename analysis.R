#devtools::install("C:/Users/mallj/GitHub/yaku-dengue/fect-edit") on first run, use this to install modified fect package
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
set.seed(0514) # make sure to always set the seed so results are replicable
source("supporting-functions.R")

### PLOT BASELINE CLIMATE, PRECIPITATION ANOMALY, AND ANOMALY DISTRIBUTION
annual_df <- read.csv("march-clim_df.csv") %>%
  # calculate mean precip in reference period% 
  mutate(date = as_date(date)) %>%
  mutate(year = year(date),
         day = day(date)) %>%
  filter(day >= 7 & day <= 20) %>%
  group_by(year, id, country) %>%
  summarize(rain = mean(rain),
            temp = mean(temp)) 

# fix ubigeos
annual_df$id[annual_df$country=="PER3"] <- str_pad(annual_df$id[annual_df$country=="PER3"], 6, pad="0")

# calculate anomaly
anomaly_df <- annual_df %>% filter(!year %in% c(2017, 2023)) %>% # remove 2017, El Nino Year
  group_by(id, country) %>%
  summarize(mean_rain = mean(rain),
            mean_temp = mean(temp)) %>%
  right_join(annual_df) %>%
  filter(year == 2023) %>%
  mutate(diff_rain = rain - mean_rain) %>% 
  ungroup()

write.csv(anomaly_df, "anomaly_df.csv", row.names=FALSE)

# Plot distribution of preciptiation anomalies across districts and regions
anomaly_df %>% filter(country %in% c("PER3", "PER")) %>%
  mutate(adm = ifelse(country=="PER3", "A. District", "B. Region")) %>%
  ggplot()+
  geom_histogram(aes(x=diff_rain*1000))+
  xlab("Precipitation Anomaly")+
  facet_wrap(~adm, scale="free_y")+
  theme_classic()

ggsave("figs/anomaly_dist.pdf", height=4, width=8, units="in")

# make maps of baseline climate conditions
map3 <- read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo) %>% 
  merge(anomaly_df %>% filter(country=="PER3"))

map3 %>%  filter(country=="PER3") %>%
  filter(diff_rain > .0085) %>%
  summarise(id = "extreme") -> extreme_map3 # treated districts most heavily impacted by cyclone

p2a <- ggplot() + 
  geom_sf(data=map3, aes(fill=diff_rain*1000)) +
  geom_sf(data = extreme_map3, col = "black", linewidth = .4)+ #bold outline of treated districts
  scale_fill_viridis("Precipitation Anomaly (mm/day)", option="H")+
  theme_void()

p2b <- ggplot() + 
  geom_sf(data=map3, aes(fill=mean_rain*1000)) +
  geom_sf(data = extreme_map3, col = "black", linewidth = .4)+ #bold outline of treated districts
  scale_fill_viridis("Historic Precipitation (mm/day)", option="mako", 
                     direction = -1)+
  theme_void()

p2c <- ggplot()+
  geom_sf(data=map3, aes(fill=mean_temp)) +
  geom_sf(data = extreme_map3, col = "black", linewidth = .4)+ #bold outline of treated districts
  scale_fill_viridis("Historic Temperature (C)", option="plasma")+
  theme_void()

plot_grid(p2a, plot_grid(p2b, p2c, nrow=2, labels=c("B", "C")), ncol=2, labels=c("A", ""))
ggsave("figs/climate_map-noext.pdf", height=8, width=8, units="in")
ggsave("figs/climate_map-noext.png", height=8, width=8, units="in")

#### PREPARE INPUT DATA FOR SYNTHETIC CONTROL ANALYSIS ####

# select districts with cases in 2023
case_df<-readRDS("PER_adm3_cases.RDS")

# identify districts with cases reported in 2023
cases_2023<-case_df %>%
  filter(year==2023) %>%
  select(ubigeo) %>%
  unlist() %>%
  unique()

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

# population dataframe from Google Earth Engine
pop_df <- read.csv("pop/PER3_pop.csv") %>%
  rename(pop = sum,
         id = ubigeo) %>%
  mutate(id = str_pad(id, 6, pad="0")) # allows us to merge with cases

# add population column and calculate incidence
case_df %<>% left_join(pop_df) %>%
  mutate(inc = cases / pop)


#### RUN MAIN ANALYSIS: MATCHING AND SYNTHETIC CONTROL, OUTPUT RESULTS  ####
# run main analysis, plot all figures, and save output with "adm3-allper" prefix
match_out_allper <- match_fun(anomaly_upper = .0085, anomaly_lower = .0085,
                              cases_2023 = cases_2023,
                              my_country = "PER3", 
                              match_num=10,
                              plot_bal=TRUE, plot_map=TRUE, file_prefix="adm3-allper",
                              map=per_map, big_map=dept_map)

synth_out_allper <- synth_fun(case_df, match_out_allper, "adm3-allper", 
                              att_plot=TRUE, spatt_plot=TRUE, map=per_map, big_map=dept_map, use_clim=TRUE,
                              start_year = 2016)

# Get table about treated, untreated, and matched control districts
match_out_allper$table

# Examine values for cyclone effects: print
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
  mutate(ATT = mid.num, CI.lower=lower.num, CI.upper=upper.num) %>%
  mutate(`Number Attributable Cases` = paste0(round(ATT), " (", round(CI.lower), " - ", round(CI.upper), ")")) %>%
  mutate(`Percent Attributable Cases` = paste0(round(mid.pct, digits=2)*100, " (", 
                                               round(lower.pct, digits=2)*100, ", ",
                                               round(upper.pct, digits=2)*100, ")"
  )) %>%
  mutate(`p-value` = round(recalc.p, digits=3)) %>%
  rename(`Reported Cases` = "obs") %>%
  mutate(`Number Non-Attributable Cases` = paste0(round(`Reported Cases` - mid.num), " (", round(`Reported Cases`- lower.num), " - ", round(`Reported Cases` - upper.num), ")")) %>%
  mutate(`Dates` = paste(start_date, "-", end_date)) %>%
  select(`Dates`, `Percent Attributable Cases`, `Number Attributable Cases`, `Number Non-Attributable Cases`, `Reported Cases`, `p-value`) %>%
  xtable(., type = "latex", row.names=FALSE, digits=c(0,0,0,0,0,0,3)) %>%
  print(file = "adm3-allper_table.tex", include.rownames=FALSE)

# Output table for covariate coefficients
synth_out_allper$gsynth_obj$est.beta

# Plot latent factor
synth_out_allper$gsynth_obj$factor %>% 
  data.frame() %>% 
  mutate(time=row_number()) %>% 
  pivot_longer(!time, names_to="Factor", names_prefix="X", values_to="Estimate") %>% 
  ggplot() + 
  geom_line(aes(x=time, y=Estimate, color=Factor, group=Factor))+
  scale_x_continuous(breaks=synth_out_allper$year_ind,
                     labels=synth_out_allper$years)+
  theme_classic()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Time")

ggsave("figs/lf-vals.pdf", height=4, width=4, units="in")


# Output values of cyclone-attributable cases per thousand and sort by that
lapply(match_out_allper$treated_names, function(x)
  synth_out_allper$gsynth_obj$group.output[[x]]$att.on %>% tail(9) %>% head(-1) %>% sum() %>% data.frame(att.cases = ., id=x)) %>%
  do.call(rbind, .) %>%
  left_join(pop_df) %>%
  mutate(att.pt = att.cases/pop * 1000) %>% 
  arrange(desc(att.pt))

#### RUN SUPPLEMENTAL ANALYSES ####
### Run for just coastal
match_out_coast <- match_fun(anomaly_upper = .0085, anomaly_lower = .0085,
                             coastal_dist = coastal_dist, cases_2023 = cases_2023,
                             match_num=10,
                             my_country = "PER3", 
                             plot_bal=FALSE, plot_map=FALSE, file_prefix="adm3-coast",
                             map=per_map, big_map=dept_map)

synth_out_coast <- synth_fun(case_df, match_out_coast, "adm3-coast",  
                             att_plot=TRUE, spatt_plot=FALSE, map=per_map, use_clim=TRUE,
                             start_year = 2016)


### Without climate covariates 
synth_out_allper_noclim <- synth_fun(case_df, match_out_allper, "adm3-allper-noclim", 
                                     use_clim=FALSE,
                                     att_plot=TRUE, spatt_plot=FALSE, map=per_map,
                                     start_year = 2016)

synth_out_coast_noclim <- synth_fun(case_df, match_out_coast, "adm3-coast-noclim", 
                                    use_clim=FALSE,
                                    att_plot=FALSE, spatt_plot=FALSE, map=per_map,
                                    start_year = 2016)

# compare no clim/with clim x coastal/all districts
i <- 1
rmse_df <- data.frame()
scen_att_df <- data.frame()

# grab number attributable cases and rmse for all models
for(df in list(synth_out_allper$gsynth_obj, synth_out_allper_noclim$gsynth_obj, 
               synth_out_coast$gsynth_obj, synth_out_coast_noclim$gsynth_obj)){
  
  # set names of models
  clim_str <- ifelse(i %% 2 == 0, "\n(- clim.)", "\n(+ clim.)")
  match_str <- ifelse(i <= 2, "All", "Coastal")
  name_str <- paste(match_str, clim_str)
  
  rmse_df %<>% rbind(., data.frame(name = name_str,
                                   RMSE = df$rmse))
  
  # will plot attributable cases through end of year
  scen_att_df <-  df$est.att %>%
    data.frame() %>%
    tail(10) %>%
    mutate(time = row_number(),
           name= name_str) %>%
    select(mid.num, obs, lower.num, upper.num, name, time) %>%
    rbind(scen_att_df)
  
  i <- i + 1
}

# plot of RMSE
scenarios_p1 <- rmse_df %>%
  ggplot()+
  geom_bar(aes(x=name, y=RMSE, fill=name), stat="identity")+
  theme_classic() + 
  theme(legend.position="none")+
  xlab("")


# make x-axis for below figure dates
epiweek_df <- data.frame(dates= seq.Date(from = as_date("2023-03-25"),
                                         to = as_date("2023-12-29"),
                                         by = "28 days"),
                         time=1:10) 

# only keep observed cases calculated across all districts (instead of just coastal)
scen_att_df %<>%
  group_by(time) %>%
  summarize(obs = max(obs)) %>%
  right_join(scen_att_df %>% select(-obs))

# plot number of attributable cases over time
scenarios_p2 <- scen_att_df %>%
  filter(time <= 10) %>%
  left_join(epiweek_df) %>%
  ggplot() +
  geom_pointrange(aes(x=dates, y=mid.num, ymin=lower.num, ymax=upper.num, color=name), # 95% confidence interval for each model 
                  size=.2, position=position_dodge(width=10))+
  geom_line(aes(y=obs, x=dates))+
  scale_color_discrete("")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+ # make nice x-axis
  theme_classic()+
  geom_hline(yintercept=0, linetype="dashed")+
  theme(legend.position="bottom")+
  ylab("Treatment Effect")+
  xlab("Time")

# combine plots and save
plot_grid(scenarios_p1, scenarios_p2+theme(legend.position="none"), rel_widths=c(1,2), labels="AUTO") %>%
  plot_grid(., get_legend(scenarios_p2), rel_heights=c(9,1), nrow=2)
ggsave("figs/comp-scenarios.pdf", height=4, width=8)

## Test effects of excluding observations prior to 2016 (by not setting start_year)
synth_out_allper_pre16 <- synth_fun(case_df, match_out_allper, "adm3-allper-pre16", 
                                    use_clim=TRUE,
                                    att_plot=TRUE)

## Test effects of varying anomaly threshold for control vs treated
# initialize variables to store values
match_anomvar <- list() 
synth_anomvar <- list()
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
                            use_clim=TRUE,
                            start_year = 2016)
      
      # store desired values for plotting
      this_df <- data.frame(anomaly_lower = toString(anomaly_lower*1000),
                            anomaly_upper = toString(anomaly_upper*1000),
                            nmatched = my_match$match_names %>% length(),
                            ntreated = my_match$treated_names %>% length(),
                            temp_bal = my_match$balance[["mean_temp"]],
                            rain_bal = my_match$balance[["mean_rain"]],
                            rmse = my_synth$gsynth_obj$rmse,
                            att.est = my_synth$att_nums$num_attr,
                            att.lower = my_synth$att_nums$lower_ci,
                            att.upper = my_synth$att_nums$upper_ci,
                            num.cases = my_synth$att_nums$num_cases %>% as.numeric())
      
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
# p12 = Abs Mean Stand Diff for Precip, p13 = RMSE
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
  geom_bar(aes(x=anomaly_upper, y=rmse, fill=anomaly_lower), stat="identity", position="dodge")+
  ggtitle("RMSE (Cases)")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  scale_fill_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  ylab("RMSE")+
  xlab("")

anom_p1 <- plot_grid(anom_p11, anom_p12, anom_p13, nrow=1) %>%
  add_sub(label="Precip. Anomaly (Control Threshold)", size=12) %>%
  ggdraw()

# plot of percent attributable cases depending on threshold, with 95% CI
anom_p2 <-  anomvar_df %>%
  ggplot(aes(x=anomaly_upper, color=anomaly_lower)) + 
  geom_pointrange(aes(y=att.est/num.cases*100, ymin=att.lower/num.cases*100, ymax=att.upper/num.cases*100),
                  stat="identity", position = position_dodge2(width = .2))+ # plot CI
  xlab("Precip. Anomaly (Treatment Threshold)")+
  ylab("Attributable \nCases (%)")+
  theme_classic()+
  scale_color_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  theme(legend.position="bottom", text=element_text(size=12))+
  geom_hline(yintercept=c(0, 100), color="grey", linetype="dotted")


# plots of sample sizes: p31 = number of control districts, p32 = number of treated districts
anom_p31 <- anomvar_df %>%
  ggplot() + 
  geom_bar(aes(x=anomaly_upper, y=nmatched, fill=anomaly_lower), stat="identity", position="dodge")+
  ggtitle("Control Districts")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  scale_fill_viridis_d("Precip. Anomaly (Control Threshold)", direction=-1)+
  ylab("n")+
  xlab("")+
  scale_y_continuous(limits=c(0, max(anomvar_df$nmatched)))

anom_p32 <- anomvar_df %>%
  ggplot() + 
  geom_bar(aes(x=anomaly_upper, y=ntreated, fill=anomaly_lower), stat="identity", position="dodge")+
  ggtitle("Treated Districts")+
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
# initialize dataframe to store values
mnvar_df <- data.frame()

for(i in 1:3){
  match_num <- c(5, 10, 15)[i]
  
  # for each value of match_num, rerun matching and synthetic control 
  match_mn <- match_fun(anomaly_upper = .0085, anomaly_lower = .0085,
                        cases_2023 = cases_2023,
                        my_country = "PER3",
                        file_prefix= NA,
                        match_num=match_num)
  
  synth_mn <- synth_fun(case_df, match_mn, NA, use_clim=TRUE,
                        start_year = 2016)
  
  # store values
  mnvar_df %<>% rbind(data.frame(match_num = match_num,
                                 nmatched = match_mn$match_names %>% length(),
                                 ntreated = match_mn$treated_names %>% length(),
                                 temp_bal = match_mn$balance[["mean_temp"]],
                                 rain_bal = match_mn$balance[["mean_rain"]],
                                 rmse = synth_mn$gsynth_obj$rmse,
                                 att.est = synth_mn$att_nums$num_attr,
                                 att.lower = synth_mn$att_nums$lower_ci,
                                 att.upper = synth_mn$att_nums$upper_ci,
                                 num.cases = synth_mn$att_nums$num_cases %>% as.numeric()))
  
  if(i == 3){
    # additional run for no matching
    i <- 4
    match_num <- "All"
    # edit the match_out dataframe to include everything in the control pool
    match_mn$match_names <- filter(match_mn$df, is_control %in% c("Untreated", "Matched Control")) %>%
      select(id) %>%
      unique() %>% 
      unlist()
    
    # conduct synthetic control analysis 
    synth_mn <- synth_fun(case_df, match_mn, NA, use_clim=TRUE) 
    
    # store values
    mnvar_df %<>% rbind(data.frame(match_num = "All",
                                   nmatched = match_mn$match_names %>% length(),
                                   ntreated = match_mn$treated_names %>% length(),
                                   temp_bal = match_mn$balance[["mean_temp"]],
                                   rain_bal = match_mn$balance[["mean_rain"]],
                                   rmse = synth_mn$gsynth_obj$rmse,
                                   att.est = synth_mn$att_nums$num_attr,
                                   att.lower = synth_mn$att_nums$lower_ci,
                                   att.upper = synth_mn$att_nums$upper_ci,
                                   num.cases = synth_mn$att_nums$num_cases %>% as.numeric()))
    
  }
}

# maintain correct order on x-axis
mnvar_df$match_num <- factor(mnvar_df$match_num, levels=c("5", "10", "15", "All"))

# in case anything was run twice by accident
mnvar_df %<>% distinct()



# these will go into the balance plot: p11 = Abs Mean Stand Diff for Temp, 
# p12 = Abs Mean Stand Diff for Precip, p13 = RMSE
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
  geom_bar(aes(x=match_num, y=rmse), stat="identity")+
  ggtitle("RMSE (Cases)")+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  ylab("Abs. Mean Stand. Diff.")+
  xlab("")

mn_p1 <- plot_grid(mn_p11, mn_p12, mn_p13, nrow=1) %>%
  add_sub(label="Matching Number", size=12) %>%
  ggdraw()

# plot of percent attributable cases depending on threshold, with 95% CI
mn_p2 <-  mnvar_df %>%
  ggplot(aes(x=as.factor(match_num))) + 
  geom_pointrange(aes(y=att.est/num.cases*100, ymin=att.lower/num.cases*100, ymax=att.upper/num.cases*100),
                  stat="identity")+
  xlab("Precip. Anomaly (Treatment Threshold)")+
  ylab("Attributable \nCases (%)")+
  theme_classic()+
  theme(legend.position="bottom")+
  geom_hline(yintercept=c(0, 100), color="grey", linetype="dotted")+
  scale_x_discrete(breaks=c("5", "10", "15", "All"))

# plots of sample sizes: p31 = number of control districts, p32 = number of treated districts
mn_p31 <- mnvar_df %>%
  ggplot() + 
  geom_bar(aes(x=match_num, y=nmatched), stat="identity")+
  ggtitle("Control Districts")+
  theme_classic()+
  theme(legend.position="none",  plot.title = element_text(size = 12))+
  ylab("n")+
  xlab("")+
  scale_y_continuous(limits=c(0, max(mnvar_df$nmatched)))

mn_p32 <- mnvar_df %>%
  ggplot() + 
  geom_bar(aes(x=match_num, y=ntreated), stat="identity")+
  ggtitle("Treated Districts")+
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
  this_synth <- synth_fun(case_df, match_out_allper, use_clim=TRUE, "adm3-lfvar", 
                          lf_num=lf_num, start_year = 2016)
  
  # store outputs
  synth_lf[[lf_num+1]] <- this_synth
  
  synth_lf_df %<>% rbind(data.frame(lf_num = lf_num,
                                    RMSE = this_synth$gsynth_obj$rmse,
                                    IC = this_synth$gsynth_obj$IC,
                                    PC = this_synth$gsynth_obj$PC,
                                    att.est = this_synth$att_nums$num_attr,
                                    att.lower = this_synth$att_nums$lower_ci,
                                    att.upper = this_synth$att_nums$upper_ci,
                                    num.cases = this_synth$att_nums$num_cases %>% as.numeric()))
  
  # keep track of where we are by printing
  print(lf_num)
}

# MSPE is hard-coded based on cross-validation printed in fitting the main model
synth_lf_df$MSPE <- c(4132, 1804, 4516, 15335, 11063, 9036)

# in case it ran twice, only show distinct entries
synth_lf_df %<>% distinct()

# plot the different balance metrics provided by the fect package as facets
lf_p1 <- synth_lf_df %>% 
  select(MSPE, IC, PC, lf_num) %>%
  pivot_longer(MSPE:PC, names_to = "bal_metric", values_to="value") %>%
  ggplot() + 
  geom_bar(aes(x=lf_num, y=value), stat="identity")+
  xlab("Number of Latent Factors")+
  facet_wrap(~bal_metric, scale="free_y",
             strip.position = "left")+
  theme_classic()+
  theme(legend.position="none")+
  ylab("")+
  theme(strip.background = element_blank(),
        strip.placement = "outside")+
  scale_x_continuous(breaks=0:5)


# plot percent attributable cases vs number of latent factors with 95% confidence intervals
lf_p2 <- ggplot(synth_lf_df, aes(x=lf_num)) + 
  geom_pointrange(aes(y=att.est/num.cases*100, ymin=att.lower/num.cases*100, ymax=att.upper/num.cases*100),
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
case_df_adm1<-read.csv("adm1_cases.csv") %>%
  left_join(per_rename) %>%
  mutate(id = ifelse(!is.na(name), name, id)) %>%
  select(-name) %>%
  mutate(inc = cases/pop) %>%
  filter(year < 2024) %>%
  filter(year < 2023 | week <= 28)

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
match_out_adm1 <- match_fun(anomaly_upper = .007, anomaly_lower = .007,
                            cases_2023 = cases_2023_adm1,
                            match_num=10,
                            my_country = c("PER", "COL1", "ECU1", "MEX", "BRA1"), 
                            plot_bal=FALSE, plot_map=FALSE, file_prefix="adm1",
                            map=five_map, big_map=country_bounds)

synth_out_adm1 <- synth_fun(case_df_adm1, match_out_adm1, "adm1",  
                            att_plot=TRUE, spatt_plot=FALSE, map=five_map, use_clim=TRUE,
                            start_year = 2018, end_week=28, method="gsynth") # smaller number of treated districts, use gsynth method for inference
