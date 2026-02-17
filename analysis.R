#install_github("mjharris95/fect") #on first run, use this to install modified fect package
library(tidyverse)
library(scales)
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
library(ggspatial)
library(ggpattern)
library(RColorBrewer)
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
                              match_num=5,
                              plot_bal=TRUE, plot_map=TRUE, 
                              file_prefix="adm3-allper",
                              map=per_map, big_map=dept_map)

synth_out_allper <- synth_fun(case_df, match_out_allper, pop_df, "adm3-allper", 
                              att_plot=TRUE, spatt_plot=TRUE, map=per_map, 
                              big_map=dept_map, use_clim=TRUE, use_r0=FALSE,
                              start_year = 2016, inc=TRUE)

# save matching and synthetic control outputs
save(match_out_allper, synth_out_allper, file="peru-main.RData")


# make maps of baseline climate conditions
map3 <- read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo) %>% 
  merge(anomaly_df %>% filter(country=="PER3"))

map3 %>%  filter(country=="PER3") %>%
  filter(id %in% match_out_allper$treated_names) %>%
  summarise(id = "extreme") -> extreme_map3 # treated districts most heavily impacted by extreme precip

# matched control districts
map3 %>%  filter(country=="PER3") %>%
  filter(id %in% match_out_allper$match_names) %>%
  summarise(id = "matched") -> match_map3 # treated districts most heavily impacted by extreme precip

# initialize signed sqrt transform (from https://andrewpwheeler.com/2015/07/31/custom-square-root-scale-with-negative-values-in-ggplot2-r/)
S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)                   
     
# merge data and plot for adm1
p2a <- ggplot() + 
  geom_sf(data=map3, aes(fill=diff_rain*1000), col=NA) +
  geom_sf(data = extreme_map3, col = "black", fill=NA, linewidth = .4)+ # bold outline of extreme precip districts
  geom_sf_pattern(data = match_map3, pattern="stripe", pattern_angle=30,
                  pattern_size=.3, pattern_density=.5, pattern_spacing=.03,
                  pattern_linetype="dotted",
                  pattern_fill=NA, pattern_colour="black",
                  fill=NA, col=NA)+ 
  scale_fill_fermenter(type="div", palette = "BrBG", 
                       limits=c(-15, 15), direction=1,
                       breaks=seq(from=-15,to=15, by=3),
                       labels=seq(from=-15,to=15, by=3) %>% ifelse(.%% 6 == 0, ., ""))+
  theme_void() +
  theme(legend.position="bottom")+
  annotation_scale(style="ticks", pad_x= unit(0, "cm"))+
  guides(fill = guide_colourbar("Precipitation Anomaly (mm/day)", 
                                title.position="top", title.hjust = 0.5,
                                barwidth=11))

p2b <- ggplot() + 
  geom_sf(data=map3, aes(fill=mean_rain*1000), col=NA) +
  geom_sf(data = extreme_map3, col = "black", fill=NA, linewidth = .4)+ # bold outline of extreme precip districts
  geom_sf_pattern(data = match_map3, pattern="stripe", pattern_angle=30,
                  pattern_size=.3, pattern_density=.5, pattern_spacing=.03,
                  pattern_linetype="dotted",
                  pattern_fill=NA, pattern_colour="black",
                  fill=NA, col=NA)+ 
  scale_fill_viridis("Historical Precipitation (mm/day)", option="viridis", 
                     direction = -1)+
  theme_void() +
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth=11))

p2c <- ggplot()+
  geom_sf(data=map3, aes(fill=mean_temp),col=NA) +
  geom_sf(data = extreme_map3, col = "black", fill=NA, linewidth = .4)+ # bold outline of extreme precip districts
  geom_sf_pattern(data = match_map3, pattern="stripe", pattern_angle=30,
                  pattern_size=.3, pattern_density=.5, pattern_spacing=.03,
                  pattern_linetype="dotted",
                  pattern_fill=NA, pattern_colour="black",
                  fill=NA, col=NA)+ 
  scale_fill_viridis("Historical Temperature (C)", option="plasma", breaks=c(5, 10, 15, 20, 25))+
  theme_void() +
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth=11))

plot_grid(p2a, p2b, p2c, nrow=1, labels="AUTO", rel_widths=c(1,1,1), align="h")
#ggsave("figs/climate_map.pdf", height=4, width=8, units="in")
ggsave("figs/climate_map.png", dpi=320, height=4, width=8, units="in")


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

# Time series: raw inc over time in extreme vs non-extreme precip 
group_pop <- rbind(pop_df %>%
                      filter(id %in% match_out_allper$treated_names) %>%
                      mutate(group = "Extreme Precip."),
                  pop_df %>%
                      filter(id %in% match_out_allper$match_names) %>%
                      mutate(group = "Matched Control"), 
                  pop_df %>% 
                      filter(id %in% setdiff(cases_2023, match_out_allper$treated_names)) %>%
                      mutate(group = "Non-extreme Precip.")) %>%
              group_by(year, group) %>%
              summarize(pop = sum(pop, na.rm=TRUE))
  

case_df_weekly <- case_df %>%
  mutate(date = ymd(paste(year, 1, 1, sep = "-")) + weeks(week - 1)) %>% 
  filter(year >= 2016)

inc_data <- rbind(case_df_weekly %>%
        filter(id %in% match_out_allper$treated_names) %>%
        mutate(group = "Extreme Precip."),
      case_df_weekly %>%
        filter(id %in% match_out_allper$match_names) %>%
        mutate(group = "Matched Control"), 
      case_df_weekly %>% 
        filter(id %in% setdiff(cases_2023, match_out_allper$treated_names)) %>%
        mutate(group = "Non-extreme Precip.")) %>%
  group_by(date, group, year) %>%
  summarize(cases = sum(cases)) %>%
  left_join(group_pop) %>%
  mutate(incidence = cases/pop*1000) %>%
  distinct()

inc_data %>% 
    filter(group == "Extreme Precip.") %>%
    group_by(year) %>%
    summarize(avg =  sprintf("%.2f", max(incidence)))  

ggplot()+
  # geom_rect(aes(xmin = seq.Date(from = as.Date("2016-01-01"), to = as.Date("2022-01-01"), by = "2 years"),
  #               xmax = seq.Date(from = as.Date("2016-12-31"), to = as.Date("2022-12-31"), by = "2 years"),
  #               ymin = -Inf, ymax = Inf), fill="grey90", colour="grey90")+
  # geom_text(data = inc_lab, aes(x=xpos, y = ypos, label=tot))+
  geom_line(data = inc_data, aes(x=date, y = incidence, group=group, color=group), lwd=.5, alpha=.6)+
  geom_point(data = inc_data %>% filter(month(date) %in% c(1,4,7,10) & day(date)<8), aes(x=date, y = incidence, group=group, color=group, pch=group))+
  ylab("Incidence (cases per thousand people)") +
  xlab("Time (years)")+
  scale_color_manual("",
                     breaks = c("Extreme Precip.", "Matched Control", "Non-extreme Precip."),
                     values = c("#581845", "#6FC0DB", "#FFC300"))+
  scale_shape_manual("",
                     breaks = c("Extreme Precip.", "Matched Control", "Non-extreme Precip."),
                     values = c(16, 17, NA))+
  theme_classic()+
  theme(legend.position="bottom")+
  geom_vline(aes(xintercept=as_date("2023-03-07")), lty="dashed", color="red")+
  guides(color = guide_legend(nrow = 2))
ggsave(paste0("figs/raw-inc.pdf"), height=11, width=11, units="cm")

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
  print(file = "figs/adm3-allper_table.tex", include.rownames=FALSE)

# Output table for covariate coefficients
synth_out_allper$gsynth_obj$est.beta

# Plot latent factors
lf_df <- synth_out_allper$gsynth_obj$factor %>% 
  data.frame() %>% 
  mutate(time=row_number()) %>% 
  pivot_longer(!time, names_to="Factor", names_prefix="X", values_to="Estimate") 

  ggplot() + 
  geom_line(data=lf_df, aes(x=time, y=Estimate, color=Factor, group=Factor, lty=Factor), alpha=.8)+
  geom_point(data=lf_df %>% filter(time %% 5 == 0), aes(x=time, y=Estimate, color=Factor, group=Factor, pch=Factor), size=1)+
  scale_x_continuous(breaks=synth_out_allper$year_ind,
                     labels=synth_out_allper$years)+
  scale_shape_manual(values=c(NA, 4, NA, NA, 15))+
  scale_linetype_manual(values=c("solid", "solid", "dashed", "dotdash", "solid"))+
  scale_color_manual(values= c("#ffb000", "#fe6100", "#dc267f", "#785ef0", "#648fff"))+
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
dist_name <- read_xlsx("maps/UBIGEODISTRITOS.XLSX") %>%
  mutate(IDDIST = str_pad(IDDIST, 6, pad="0")) %>%
  rename(id = IDDIST)

lapply(match_out_allper$treated_names, function(x)
  synth_out_allper$gsynth_obj$group.output[[x]]$att.on %>% 
    tail(9) %>% 
    head(-1) %>% 
    data.frame(att.inc = sum(.)*1000, 
               peak.inc = max(.)*1000, 
               when.max = which.max(.), 
               id=x) %>%
               select(att.inc, peak.inc, when.max, id)) %>% 

  do.call(rbind, .) %>%
  arrange(desc(att.inc)) %>%
  left_join(dist_name) %>%
  distinct()

# for slides: plot difference zoomed in

epiweek_df <- data.frame(dates= seq.Date(from = as_date("2023-03-25"),
                                         to = as_date("2023-12-02"),
                                         by = "28 days"),
                         step=1:10) 

synth_out_allper$gsynth_obj$est.att %>%
  data.frame() %>%
  tail(10) %>%
  mutate(step = row_number()) %>%
  left_join(epiweek_df) %>%
  ggplot() + 
  geom_ribbon(aes(x=dates, ymax=upper.cases, ymin=lower.cases), fill="grey70")+
  geom_line(aes(x=dates, y=mid.cases))+
  geom_point(aes(x=dates, y=mid.cases))+
  theme_classic()+
  scale_x_date(date_breaks = "2 months", date_labels = "%b")+
  geom_hline(aes(yintercept=0))

ggsave("figs/diff-plot-zoomed.pdf", height=4, width=4, units="in")


#### RUN SUPPLEMENTAL SYNTHETIC CONTROL ANALYSES ####
### Run for just coastal
match_out_coast <- match_fun(anomaly_upper = .0085, anomaly_lower = .007,
                             coastal_dist = coastal_dist, cases_2023 = cases_2023,
                             match_num=5,
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
                            match_num=5)
      
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
  %>% 
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

# identify and exclude district with especially large factor loadings for factor 1 & 2
data.frame("Loading" = synth_out_allper$gsynth_obj$lambda,
           "id" = synth_out_allper$gsynth_obj$id) %>%
arrange(desc(abs(Loading.1)))

read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo) %>%
  mutate(is_focus = ifelse(id == "170103", TRUE, FALSE)) %>%
  ggplot() +
  geom_sf(aes(fill=is_focus), col="black")+
  scale_fill_manual(values=c("white", "red")) +
  theme_map()+
  theme(legend.position="none")

ggsave(paste0("figs/removedis-map.pdf"), height=20, width=20, units="cm")

match_removedis <- match_out_allper
match_removedis$match_names <- setdiff(match_removedis$match_names, "170103")

synth_out_removedis <- synth_fun(case_df, match_removedis, pop_df, "adm3-removedis", 
                              att_plot=TRUE, spatt_plot=FALSE, map=NA, 
                              big_map=dept_map, use_clim=TRUE, use_r0=FALSE,
                              start_year = 2016, inc=TRUE)

lf_df_removedis <- synth_out_removedis$gsynth_obj$factor %>% 
  data.frame() %>% 
  mutate(time=row_number()) %>% 
  pivot_longer(!time, names_to="Factor", names_prefix="X", values_to="Estimate") 

ggplot() + 
  geom_line(data=lf_df_removedis, aes(x=time, y=Estimate, color=Factor, group=Factor, lty=Factor), alpha=.8)+
  geom_point(data=lf_df_removedis %>% filter(time %% 5 == 0), aes(x=time, y=Estimate, color=Factor, group=Factor, pch=Factor), size=1)+
  scale_x_continuous(breaks=synth_out_allper$year_ind,
                     labels=synth_out_allper$years)+
  scale_shape_manual(values=c(NA, 4, NA, NA, 15))+
  scale_linetype_manual(values=c("solid", "solid", "dashed", "dotdash", "solid"))+
  scale_color_manual(values= c("#ffb000", "#fe6100", "#dc267f", "#785ef0", "#648fff"))+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Time")

ggsave("figs/lf-vals-removedis.pdf", height=4, width=4, units="in")

# plot factor loadings on a map
lp_rd <- list()
for(i in 1:5){
  lp_rd[[i]] <- data.frame("Loading" = synth_out_removedis$gsynth_obj$lambda[,i],
                        "id" = synth_out_removedis$gsynth_obj$id) %>%
    merge(per_map, .) %>%
    ggplot()+
    geom_sf(aes(fill=Loading), color="grey40", size=.01)+
    geom_sf(data=dept_map, fill=NA, color="black", size=.01)+
    theme_map()+
    ggtitle(paste0("Latent Factor ",i))+
    scale_fill_continuous_diverging(palette = "Purple-Green")
}

plot_grid(lp_rd[[1]], 
          lp_rd[[2]],
          lp_rd[[3]],
          lp_rd[[4]],
          lp_rd[[5]], nrow=3)

ggsave("figs/fl-maps-removedis.pdf", height=8, width=8, units="in")

#### run analysis excluding control districts with negative precipitation anomalies
nonneg <-   anomaly_df %>% 
              filter(diff_rain > 0 ) %>%
              select(id)  %>% 
              unlist()

match_out_nonneg <- match_fun(anomaly_upper = .0085, anomaly_lower = .007,
                              cases_2023 = cases_2023,
                              coastal_dist = nonneg,
                              my_country = "PER3", 
                              match_num=5,
                              plot_bal=FALSE, plot_map=FALSE, 
                              file_prefix="adm3-nonneg",
                              map=per_map, big_map=dept_map)

synth_out_nonneg <- synth_fun(case_df, match_out_nonneg, pop_df, "adm3-nonneg", 
                              att_plot=TRUE, spatt_plot=FALSE, map=per_map, 
                              big_map=dept_map, use_clim=TRUE, use_r0=FALSE,
                              start_year = 2016, inc=TRUE)

#### ANALYSIS FOR PART 2: SOCIOVULNERABILITY INDICES BEGINS HERE ####
# would recommend detaching other packages due to interference between dplyr and terra select
library(tidyverse)
library(magrittr)
library(raster)
library(sf)
library(terra)
library(psych)
library(reshape2)
library(colorspace)
library(cowplot)
library(xtable)
library(readxl)
# read in shapefile of districts
per_map<-read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo)

# the cov directory contains .tif files of different vulnerability indices - excluded from
# public respository for privacy reasons
files <- dir("cov")

#exclude population density
files <- files[!startsWith(files, "r_dpob")]

covar_df <- c()
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
    rbind(covar_df)
  
  
  # this loop takes a while to run, save each step
  #save(covar_df, file="covar_df.RData")
}


load("covar_df.RData")

per_map<-read_sf("maps/CDC_Distritos.shp") %>%
  rename(id = ubigeo)

# read in, combine population density files
files <- dir("cov")[startsWith(dir("cov/"), "r_dpob")]

dpop <- lapply(files, function(file) rast(paste0("cov/", file))) %>%
  do.call(merge, .) %>%
  project(per_map) 

# read flood susceptibility file
flood_sus <- rast("cov/Susceptibilidad a inundaciones1.tif") %>%
  project(., per_map)

dpop2 <- crop(dpop, flood_sus) %>%
  resample(flood_sus, method="bilinear")


# weight covariates based on population
flood_mult <- flood_sus * dpop2

# get population-weighted sum
flood_sum <- terra::extract(flood_mult, per_map,
                            fun="sum", na.rm=TRUE,
                            bind = TRUE) %>%
  st_drop_geometry() %>%
  data.frame() %>%
  rename(var = 5)

# get population and calculate pop-weighted mean
flood_df <- terra::extract(dpop2, per_map,
                           fun="sum", na.rm=TRUE,
                           bind = TRUE) %>% 
  st_drop_geometry %>%
  data.frame() %>%
  rename(pop = 5) %>%
  left_join(flood_sum) %>%
  mutate(avg_socio = var/pop) %>%
  mutate(var_name = "flood") %>%
  dplyr::filter(id %in% match_out_allper$treated_names)

# read in land use shapefile
per_map %<>% filter(id %in% cases_2023)

lu_map <- rast("cov/landuse/peru_coverage_2022.tif") %>%
  project(., per_map)

land_types <- freq(lu_map)$value

# align the rasters
dpop <- project(dpop, per_map) %>%
        crop(lu_map) %>%
        extend(lu_map) %>%
        resample(lu_map, method = "near")

# population in each type of land type
lu_mask <- lapply(land_types, function(type) ifel(lu_map == type, dpop, 0)) %>%
            rast()

names(lu_mask) <- sapply(land_types, function (type) paste0("LU_", type))

lu_pop <- lu_mask %>%
            terra::zonal(vect(per_map), "sum", na.rm = TRUE)
lu_pop$id <- per_map$id

#write.csv(lu_pop, "pop/lu_pop.csv")

dpopsq <- dpop*dpop 
  
popsq_df <- terra::extract(dpopsq, per_map,
                         fun="sum", na.rm=TRUE,
                         bind = TRUE) %>%
  st_drop_geometry() %>%
  data.frame() %>%
  mutate(var = "pop_sq")

pop_df <- dpop %>%
  terra::extract(per_map,
                 fun="sum", na.rm=TRUE,
                 bind = TRUE) %>%
  st_drop_geometry() %>%
  data.frame() %>%
  mutate(var = "pop") %>%
  rename(val = dpop_AMAZONAS)

popsq_df %<>% 
  left_join(dplyr::select(pop_df, id, val)) %>%
  mutate(avg_socio = dpob_AMAZONAS/val)

save(pop_df, popsq_df, file="pop/pop_tomatch.RData")

lu_pop <- read.csv("pop/lu_pop.csv") %>%
          mutate(id = str_pad(id, 6, pad="0")) 

lu_pop <- pop_df %>% 
          dplyr::select(id, val) %>%
          rename(pop = val) %>%
          left_join(lu_pop) 
  

lu_dict <- data.frame(name = c("prop_LU_0", "prop_LU_11", "prop_LU_12", "prop_LU_13", "prop_LU_15",
                               "prop_LU_18", "prop_LU_21", "prop_LU_24", 
                               "prop_LU_25", "prop_LU_27",
                               "prop_LU_3", "prop_LU_30", "prop_LU_31", "prop_LU_32", "prop_LU_33",
                               "prop_LU_34", "prop_LU_35", "prop_LU_4", "prop_LU_5", "prop_LU_6", 
                               "prop_LU_9", "prop_LU_21new"),
                      lu_type = c("??", "Flood Grassland/Scrubland", "Grassland", "Scrubland", "Pasture",
                                  "Agriculture", "Agriculture/Pasture Mosaic", "Infrastructure",
                                  "Non-vegetated (Other)", "Not Observed",
                                  "Forest", "Mining", "Aquaculture", "Salt Flat", "River/Lake",
                                  "Glacier", "Oil Palm", "Dry Forest", "Mangrove", "Flooded Forest", 
                                  "Forest Plantation", "Agriculture and Pasture"))
covar_df <- lu_pop %>%
  data.frame() %>%
  dplyr::select(starts_with("LU") | starts_with("id")) %>%
  rowwise() %>%
  mutate(pop = sum(c_across(LU_0:LU_35), na.rm=TRUE), # Sum across population in each land use type
         across(LU_0:LU_35, ~ . / pop, .names = "prop_{col}") # Divide each by the sum
  ) %>%
  ungroup() %>%
  mutate_at(vars(starts_with("LU")), as.numeric) %>%
  filter(id != "200604") %>%
  mutate(prop_LU_21new = prop_LU_21 + prop_LU_15 + prop_LU_18) %>%
  dplyr::select(c("prop_LU_21new", "prop_LU_25", "prop_LU_24", "prop_LU_4", "id")) %>%
  pivot_longer(-id) %>%
  left_join(lu_dict) %>%
  filter(lu_type %in% c("Agriculture and Pasture", 
                        "Dry Forest", 
                        "Infrastructure",
                        "Non-vegetated (Other)")) %>%
  # make sure we can bind this to the existing covar_df 
  rename(avg_socio = value,
         var_name = lu_type) %>%
  mutate(pop = NA,
         DISTRITO = NA,
         PROVINCIA = NA,
         DEPARTAMEN = NA,
         var = NA) %>%
  dplyr::select(-name) %>%
  dplyr::filter(id %in% match_out_allper$treated_names) %>% 
  rbind(covar_df)

#save(covar_df, file="covar_df.RData")

# load in the matching and gsynth results
load("peru-main.RData")
load("covar_df.RData")

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
  filter(!is.nan(vui)) %>%
  dplyr::select(-c(socio_vpr, socio_vps, socio_vtc, vui, "Non-vegetated (Other)")) %>%
  rename("Agriculture and pasture" = "Agriculture and Pasture")


#plot correlations between indices
covar_df %>% 
  dplyr::select(-"id") %>%
  cor() %>%
  reshape2::melt() %>%
  mutate(Var1 = recode(Var1, 
                       "rain" = "Precipitation", 
                       "flood" = "Flood susceptibility",
                       "socio_vpr" = "Low-quality walls",
                       "socio_vtc" = "Low-quality roofs",
                       "temp" = "Temperature",
                       "socio_vps" = "Low-quality floors",
                       "socio_vtp" = "Low-quality housing",
                       "socio_acc" = "Nonpublic \nwater source",
                       "hidro" = "Distance to \nwater",
                       "socio_arg" = "Inconsistent \nwater access",
                       "socio_vhc" = "Household overcrowding", 
                       "vias" = "Distance to \n roads"),
         Var2 =  recode(Var2, 
                        "rain" = "Precipitation", 
                        "flood" = "Flood susceptibility",
                        "socio_vpr" = "Low-quality walls",
                        "socio_vtc" = "Low-quality roofs",
                        "temp" = "Temperature",
                        "socio_vps" = "Low-quality floors",
                        "socio_vtp" = "Low-quality housing",
                        "socio_acc" = "Nonpublic \nwater source",
                        "hidro" = "Distance to \nwater",
                        "socio_arg" = "Inconsistent \nwater access",
                        "socio_vhc" = "Household overcrowding", 
                        "vias" = "Distance to \n roads")) %>%
  mutate(value = ifelse(value==1, NA, value)) %>%
  rename(Correlation = value) %>%
  ggplot(aes(x=Var1, y=Var2, fill=Correlation)) +
  geom_tile()+
  xlab("")+
  ylab("")+
  scale_fill_binned_diverging(breaks=seq(from=-1, to=1, by=.25), p1=2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(paste0("figs/covar-cor.pdf"), height=20, width=20, units="cm")

# plot to illustrate relative proportion of different land use types
name_dict <- read_xlsx("maps/UBIGEODISTRITOS.XLSX") %>%
  mutate(IDDIST = str_pad(IDDIST, 6, pad="0")) %>%
  rename(id = IDDIST) %>%
  select(id, NOMBDIST)
  
covar_df %>%
  dplyr::select(c("Agriculture and pasture", "Dry Forest", "Infrastructure", "id")) %>%
  pivot_longer(-id) %>%
  mutate(name = factor(name, levels = c("Agriculture and pasture", "Dry Forest", "Infrastructure"))) %>%

  ggplot() +
  geom_col(aes(y=as.factor(id), x=value, fill=name)) +
  scale_y_discrete("District", breaks=name_dict$id, labels=str_to_title(name_dict$NOMBDIST))+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous("Population Proportion")+
  scale_fill_viridis_d("Land Type",
                       breaks = c("Agriculture and pasture", "Dry Forest", "Infrastructure"))+
  guides(fill = guide_legend(reverse = TRUE)) 
  

ggsave(paste0("figs/lu-prop.pdf"), height=25, width=20, units="cm")


# determine number of factors to use based on vss
covar_df %>% 
  dplyr::select(-c(id)) %>%
  vss(n=8, rotate="varimax", fm="mle")

# run pca, store results
covar_df %>% 
  dplyr::select(-c(id)) %>%
  principal(nfactors=3, rotate="varimax", scores=TRUE) -> pca_res

# make a table of factor loadings
pca_res$loadings %>% 
  unclass %>% 
  as.data.frame %>% 
  rownames_to_column() %>%
  mutate(rowname = recode(rowname, 
        "rain" = "Precipitation", 
        "flood" = "Flood susceptibility",
        "socio_vpr" = "Low-quality walls",
        "socio_vtc" = "Low-quality roofs",
        "temp" = "Temperature",
        "socio_vps" = "Low-quality floors",
        "socio_vtp" = "Low-quality housing",
        "socio_acc" = "Nonpublic water source",
        "hidro" = "Distance to \nbody of water",
        "socio_arg" = "Inconsistent water access",
        "socio_vhc" = "Household overcrowding", 
        "vias" = "Distance to roads")) %>%
  rename(" " = rowname) %>%
  arrange(desc(RC1)) %>%
  mutate(RC1 = round(RC1, digits=2),
         RC2 = round(RC2, digits=2),
          RC3 = round(RC3, digits=2)) %>%
  mutate(RC1 = ifelse(abs(RC1)>= .6, paste0("textbf{", RC1, "}"), RC1),
         RC2 = ifelse(abs(RC2)>= .6, paste0("textbf{", RC2, "}"), RC2),
         RC3 = ifelse(abs(RC3)>= .6, paste0("textbf{", RC3, "}"), RC3)) %>%
  xtable(., type = "latex", row.names=FALSE) %>%
  print(file = "figs/rc-loadings.tex", include.rownames=FALSE)

# extract labels for main variables in each rotated components,
# use this to understand rotated components & helper for plotting
RC_labs <- pca_res$loadings %>% 
  unclass %>% 
  as.data.frame %>% 
  rownames_to_column() %>% 
  pivot_longer(starts_with("RC")) %>% 
  group_by(name) %>%
  slice_max(abs(value), n=2) %>%
  # make this into the correct variable names
  mutate(lab = recode(rowname, 
                      "rain" = "Precipitation", 
                      "flood" = "Flood susceptibility",
                      "socio_vpr" = "Low-quality walls",
                      "socio_vtc" = "Low-quality roofs",
                      "temp" = "Temperature",
                      "socio_vps" = "Low-quality floors",
                      "socio_vtp" = "Low-quality housing",
                      "socio_acc" = "Nonpublic \nwater source",
                      "socio_arg" = "Inconsistent \nwater access",
                      "hidro" = "Distance to \nbody of water",
                      "socio_vhc" = "Household overcrowding")) %>%
  mutate(lab = paste0( #ifelse(value < 0, "- ", "+ "), 
    lab, " (",  sprintf("%.2f", round(value, 2)), ")")) %>% 
  group_by(name) %>% 
  arrange(name, desc(abs(value))) %>% 
  summarise(lab = paste(lab, collapse = "\n")) %>% 
  mutate(xval = as.numeric(gsub("RC", "", name))) 

# add RC values to dataframe
pca_res$scores %>%
  data.frame() %>%
  cbind(covar_df, .) -> covar_df

# extract attributable incidence in each district across the post-cyclone period 
eff_ind <- which(rownames(synth_out_allper$gsynth_obj$est.att) %in% c("3", "4", "5"))

sapply(covar_df$id, function(dist_id) synth_out_allper$gsynth_obj$eff[eff_ind, which(synth_out_allper$gsynth_obj$id==dist_id)] %>% sum()) %>%
  data.frame(ATT = .,
             id = names(.)) %>%
  right_join(covar_df) -> covar_df

# fit regression 
mod.res <- lm(ATT~RC1+RC2+RC3, data=covar_df)
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
  slice_sample(n=611) %>%
  ungroup()

# for each bootstrap, refit the model and store the coefficients 
# here, we're resampling across both extreme precip districts and their bootstrapped estimates
suppressWarnings(boot_mod <- lapply(1:1000, function(i) slice_sample(boot_sample, n=48, replace=TRUE) %>%
                                      left_join(., covar_df %>% dplyr::select(-ATT), by="id") %>%
                                      lm(ATT~RC1+RC2+RC3, data=.)  %>% 
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

RC_labs$y <- c(20, 20, 20)

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
                 color="black", alpha=.4, linewidth=.3)+
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


#### ADDITIONAL SUPPLEMENTAL ANALYSIS: MATCHING ON LAND USE OR POP DENSITY
detach("package:terra", unload = TRUE)
detach("package:raster", unload = TRUE)
load("covar_df.RData")

match_extra <- popsq_df %>%
  select(id,avg_socio) %>%
  rename(dpop = avg_socio)
  
match_extra <- covar_df %>%
  filter(var_name == "Infrastructure") %>%
  select(id, avg_socio) %>%
  rename("Infrastructure" = avg_socio) %>%
  right_join(match_extra) %>%
  filter(id %in% cases_2023)

match_out_allper_extravars <- match_fun(anomaly_upper = .0085, anomaly_lower = .007,
                              cases_2023 = cases_2023,
                              extra_vars = match_extra,
                              my_country = "PER3", 
                              match_num=5,
                              plot_bal=TRUE, plot_map=TRUE, 
                              file_prefix="adm3-allper_extravar",
                              map=per_map, big_map=dept_map)


synth_out_matchextra <- synth_fun(case_df, match_out_allper_extravars, pop_df, "adm3-allper_extravar", 
                              att_plot=TRUE, spatt_plot=TRUE, map=per_map, 
                              big_map=dept_map, use_clim=TRUE, use_r0=FALSE,
                              start_year = 2016, inc=TRUE)
