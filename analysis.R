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
source("new-plots.R")

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
  tail(10) %>%
  data.frame() %>%
  mutate(time = as.numeric(rownames(synth_out_allper$gsynth_obj$est.att) %>% tail(10)),
         ind = row_number()) %>% 
  mutate(start_date = as_date("2023-03-25")+(ind-1)*28) %>%
  mutate(end_date = start_date + 27) %>%
  mutate(start_date  = format(start_date, "%b %d"),
         end_date = format(end_date, "%b %d")) %>%
  mutate(ATT = mid.num, CI.lower=lower.num, CI.upper=upper.num) %>%
  mutate(`Treatment Effect` = paste0(round(ATT), " (", round(CI.lower), " - ", round(CI.upper), ")")) %>%
  mutate(`Percent Attributable Cases` = paste0(round(mid.pct, digits=2)*100, " (", 
                                               round(lower.pct, digits=2)*100, ", ",
                                               round(upper.pct, digits=2)*100, ")"
  )) %>%
  mutate(`p-value` = round(recalc.p, digits=3)) %>%
  rename(`Reported Cases` = "obs") %>%
  mutate(`Dates` = paste(start_date, "-", end_date)) %>%
  select(`Dates`, `Treatment Effect`, `Reported Cases`, `Percent Attributable Cases`, `p-value`) %>%
  xtable(., type = "latex", row.names=FALSE, digits=c(0, 0,0,0,0,3)) %>%
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
