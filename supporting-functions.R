#### STANDARDIZED DIFFERENCE PLOT ####

# Returns a plot of the standardized difference between climate covariates in treated 
# and scatterplot of mean temperature vs precipitation
# in cyclone-affected vs -unaffected (before and afterr matching) districts
#
# Inputs: match_out: output from the match_fun function
#         cyclone_step: index when cyclone occurred (can be retrieved from match_fun)
#         file_prefix: prefix to save pdf image
#         years: the years over which climate data is provided (can be retrieved from match_fun)
#         year_ind: the indices for when a new year starts (can be retrieved from match_fun)
#
# Output: a pdf figures in the figs folder with suffix clim-stddiff and clim_matchscatter
balance_plot <- function(match_out, cyclone_step, file_prefix, years, year_ind){
  # Scatterplot of mean temperature vs precipitation prior to matching 
   scatter_bal_df <- match_out$df %>%
                        filter(is_control == "Matched Control") %>%
                        mutate(is_control = "Cyclone-unaffected") %>%
                        rbind(match_out$df) %>%
                        filter(step < cyclone_step) %>%
                        group_by(is_control, step) %>%
                        summarize(wmean_rain = weighted.mean(mean_rain, weight),
                                  wmean_temp = weighted.mean(mean_temp, weight)) 
  
  ggplot() + 
    geom_point(data = scatter_bal_df,
               aes(x=wmean_temp, y=wmean_rain, color=is_control)) +
    stat_smooth(data = scatter_bal_df %>% filter(is_control=="Cyclone-affected"),
                aes(y=wmean_rain, x=wmean_temp), 
                method = "lm", 
                formula = y ~ x, 
                geom = "smooth",
                color="black") +
    theme_classic()+
    ylab("Precipitation")+
    xlab("Temperature")+
    theme(legend.position="bottom")+
    scale_color_manual("",
                       breaks = c("Cyclone-affected", "Matched Control", "Cyclone-unaffected", "No Cases in 2023, Excluded", "Buffer District", "Non-coastal"),
                       values = c("#581845", "#6FC0DB", "#FFC300", "grey90", "grey50", "maroon"))
  
  ggsave(paste0("figs/", file_prefix, "clim_matchscatter.pdf"), height=6, width=8, units="in")

  # Time series plots of mean differences as time series by each district
  baldf <- match_out$df %>%
    filter(step <= cyclone_step)
  
  # calculate the differences before matching
  prediff_df <-  lapply(1:length(match_out$treated_names), function(n)
    filter(baldf, !is_control == "Cyclone-affected") %>%
      select(id, step, date_end, mean_rain, mean_temp) %>%
      left_join(., baldf %>% 
                  filter(id == match_out$treated_names[n]) %>%
                  select(id, step, date_end, mean_rain, mean_temp) %>%
                  rename(treated_rain = mean_rain,
                         treated_temp = mean_temp,
                         treated_id = id), by=c("step", "date_end"))  %>%
      mutate(rain_diff = treated_rain - mean_rain,
             temp_diff = treated_temp - mean_temp)) %>%
    do.call(rbind, .) %>%
    group_by(treated_id, step) %>%
    dplyr::summarize(rain_diff = mean(rain_diff),
                     temp_diff = mean(temp_diff)) 
  # calculate the differences after matching
  diff_df <-  lapply(1:length(match_out$treated_names), function(n)
    filter(baldf, place %in% match_out$match_list[[n]]) %>%
      select(id, step, date_end, mean_rain, mean_temp) %>%
      left_join(., baldf %>% 
                  filter(id == match_out$treated_names[n]) %>%
                  select(id, step, date_end, mean_rain, mean_temp) %>%
                  rename(treated_rain = mean_rain,
                         treated_temp = mean_temp,
                         treated_id = id), by=c("step", "date_end"))  %>%
      mutate(rain_diff = treated_rain - mean_rain,
             temp_diff = treated_temp - mean_temp)) %>%
    do.call(rbind, .) %>%
    group_by(treated_id, step) %>%
    dplyr::summarize(rain_diff = mean(rain_diff),
                     temp_diff = mean(temp_diff)) 
  
  
  # plots: each line is the mean standardized difference between a district and its matched controls
  # rain, pre-matching
  bal1 <- ggplot(prediff_df) +
    geom_line(aes(x=step, y=rain_diff/sd(baldf$mean_rain), group=treated_id), alpha=.1)+
    stat_summary(aes(x=step, y = rain_diff/sd(baldf$mean_rain)), fun.y=mean, colour="black", geom="line", lwd=.8)+
    theme_classic()+
    ylab("Std. Diff.")+
    scale_y_continuous(limits=c(-3, 5))+
    geom_hline(yintercept=0, color="red", linetype="dashed")+
    scale_x_continuous(breaks=year_ind, labels=years)+
    xlab("Time")
  
  # rain, post-matching
  bal2 <- ggplot(diff_df) +
    geom_line(aes(x=step, y=rain_diff/sd(baldf$mean_rain), group=treated_id), alpha=.1)+
    stat_summary(aes(x=step, y = rain_diff/sd(baldf$mean_rain)), fun.y=mean, colour="black", geom="line", lwd=.8)+
    theme_classic()+
    ylab("Std. Diff.")+
    scale_y_continuous(limits=c(-3, 5))+
    geom_hline(yintercept=0, color="red", linetype="dashed")+
    scale_x_continuous(breaks=year_ind, labels=years)+
    xlab("Time")
  
  # temperature, pre-matching
  bal3 <- ggplot(prediff_df) +
    geom_line(aes(x=step, y=temp_diff/sd(baldf$mean_temp), group=treated_id), alpha=.1)+
    stat_summary(aes(x=step, y = temp_diff/sd(baldf$mean_temp)), fun.y=mean, colour="black", geom="line", lwd=.8)+
    theme_classic()+
    ylab("Std. Diff")+
    scale_y_continuous(limits=c(-2.5, 1.5))+
    geom_hline(yintercept=0, color="red", linetype="dashed")+
    scale_x_continuous(breaks=year_ind, labels=years)+
    xlab("Time")
  
  # temperature, post-matching
  bal4 <- ggplot(diff_df) +
    geom_line(aes(x=step, y=temp_diff/sd(baldf$mean_temp), group=treated_id), alpha=.1)+
    stat_summary(aes(x=step, y = temp_diff/sd(baldf$mean_temp)), fun.y=mean, colour="black", geom="line", lwd=.8)+
    theme_classic()+
    ylab("Std. Diff")+
    scale_y_continuous(limits=c(-2.5, 1.5))+
    geom_hline(yintercept=0, color="red", linetype="dashed")+
    scale_x_continuous(breaks=year_ind, labels=years)+
    xlab("Time")
  
  plot_grid(bal1, bal2, bal3, bal4, nrow=2, scale=.9,
            labels = c("A. Precipitation (Pre-Match)",
                       "B. Precipitation (Matched)",
                       "C. Temperature (Pre-Match)",
                       "D. Temperature (Matched)"),
            hjust=-.1, label_size= 12, vjust=1)
  
  ggsave(paste0("figs/", file_prefix, "clim-stddiff.pdf"), height=8, width=8, units="in")
}

#### CLIMATE TIME SERIES PLOT ####

# Returns a plot of mean climate conditions over time in the treated, Cyclone-unaffected,
# and matched control districts
#
# Inputs: match_out: output from the match_fun function
#         cyclone_step: index when cyclone occurred (can be retrieved from match_fun)
#         file_prefix: prefix to save pdf image
#         years: the years over which climate data is provided (can be retrieved from match_fun)
#         year_ind: the indices for when a new year starts (can be retrieved from match_fun)
#         my_vars: which climate covariates to plot (options: Temperature, Preciptiation, Rel_R0)
# Output: a pdf figure in the figs folder with suffix -clim_matchcomp

climts_plot <- function(match_out, cyclone_step, file_prefix, 
                        years, year_ind, my_vars = c("Temperature", "Precipitation")) {
  
  match_df <- match_out$df
  
    
  clim_ts <- match_df %>%
    # also include the matched control districts in the Cyclone-unaffected pool%
    filter(is_control == "Matched Control") %>%
    mutate(is_control = "Cyclone-unaffected") %>%
    rbind(match_df) %>%
    group_by(is_control, step) %>%
    # take a weighted mean (some matched control districts are included multiple times)
    dplyr::summarize(Temperature = weighted.mean(mean_temp, weight),
                     Precipitation = weighted.mean(mean_rain, weight),
                     Rel_R0 = weighted.mean(mean_rel_r0, weight),
    ) 

    # plot temperature, precipitation, and relative r0
  temp_plot <-  clim_ts %>% 
      ggplot() + 
      # highlight four-week period containing the cyclone
      geom_rect(xmin = cyclone_step, xmax  = cyclone_step + 1, ymin=-Inf, ymax=Inf, alpha=.5, fill="grey70", color="grey70")+
      geom_line(aes(x=step, y=Temperature, group=is_control, color=is_control), alpha=.8)+
      scale_color_manual("",
                         breaks = c("Cyclone-affected", "Matched Control", "Cyclone-unaffected", "No Cases in 2023, Excluded", "Buffer District", "Non-coastal"),
                         values = c("#581845", "#6FC0DB", "#FFC300", "grey90", "grey50", "maroon"))+
      theme_classic()+
      xlab("Year")+
      scale_x_continuous(breaks=year_ind, # make x-axis years
                         labels=years)+
      ylab("Temperature (C)") +
      theme(legend.position="bottom", 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
    
  rain_plot <-  clim_ts %>% 
    ggplot() + 
    geom_rect(xmin = cyclone_step, xmax  = cyclone_step + 1, ymin=-Inf, ymax=Inf, alpha=.5, fill="grey70", color="grey70")+
    geom_line(aes(x=step, y=Precipitation, group=is_control, color=is_control), alpha=.8)+
    scale_color_manual("",
                       breaks = c("Cyclone-affected", "Matched Control", "Cyclone-unaffected", "No Cases in 2023, Excluded", "Buffer District", "Non-coastal"),
                       values = c("#581845", "#6FC0DB", "#FFC300", "grey90", "grey50", "maroon"))+
    theme_classic()+
    xlab("Year")+
    scale_x_continuous(breaks=year_ind,
                       labels=years)+
    ylab("Precipitation (mm/day)") +
    theme(legend.position="none", 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  # temperature-dependent R0 plot
  r0_plot <-  clim_ts %>%
    ggplot() + 
    geom_rect(xmin = cyclone_step, xmax  = cyclone_step + 1, ymin=-Inf, ymax=Inf, alpha=.5, fill="grey70", color="grey70")+
    geom_line(aes(x=step, y=Rel_R0, group=is_control, color=is_control), alpha=.8)+
    scale_color_manual("",
                       breaks = c("Cyclone-affected", "Matched Control", "Cyclone-unaffected", "No Cases in 2023, Excluded", "Buffer District", "Non-coastal"),
                       values = c("#581845", "#6FC0DB", "#FFC300", "grey90", "grey50", "maroon"))+
    theme_classic()+
    xlab("Year")+
    scale_x_continuous(breaks=year_ind,
                       labels=years)+
    ylab("Relative R0")+
    theme(legend.position="none", 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
    # plot a grid of whichever climate variables were requested
    plots<-list()
  
    leg <- get_legend(temp_plot)
    temp_plot <- temp_plot+theme(legend.position="none")
    
    if("Temperature" %in% my_vars){
      plots[[length(plots)+1]] <-temp_plot
    }
    
    if("Precipitation" %in% my_vars){
      plots[[length(plots)+1]] <- rain_plot
    }
    
    
    if("Relative R0" %in% my_vars){
      plots[[length(plots)+1]] <- r0_plot
    }
    
    # set dimensions based on however many subplots were generated
    if(length(plots)==1){
      p <- plot_grid(plots[[1]]) %>%
        plot_grid(., leg, nrow=2, rel_heights=c(9, 1))
    }
    
    else if(length(plots)==2){
      p <- plot_grid(plots[[1]], plots[[2]], nrow=2) %>%
        plot_grid(., leg, nrow=2, rel_heights=c(9, 1))
    }
    
    else{
      p <- plot_grid(plots[[1]], plots[[2]], plots[[3]], nrow=3) %>%
        plot_grid(., leg, nrow=2, rel_heights=c(9, 1))
    }
    
    print(p)
    
    ggsave(paste0("figs/", file_prefix, "-clim_matchcomp.pdf"), height=6, width=8, units="in")

}

#### DISTRICT MAP FOR MATCHING  ####

# Returns a plot of districts color-coded by Cyclone-affected, Cyclone-unaffected, matched control,
# or reason for exclusions from analysis
#
# Inputs: map: shapefile of spatial units at level used for analysis
#         big_map: shapefile of spatial units at higher administrative division to indicate borders
#         file_prefix: prefix to save pdf image
#         match_out: output from the match_fun function
#         buffer_zone: optional, vector indicating spatial units with precipitation anomalies between upper and lower threshold
#         cases_2023: optional, vector indicating spatial units with cases reported in 2023 (if units not in this list were excluded)
#         coastal_dist: optional, vector indicating spatial units that are coastal (if units not in this list were excluded)
#         note that spatial ids provided for buffer_zone, cases_2023, and coastal_dist should match ids in map file
# Output: a pdf figure in the figs folder with suffix -clim_matchcomp

matchmap_plot <- function(map, big_map, file_prefix, match_out, 
                          buffer_zone = c(), cases_2023 = c(), coastal_dist = c()){
  
  # since id columns are different for adm3 and adm1 analysis, rename them
  map %<>% rename(any_of(rename_list))
  
  match_df <- match_out$df
  
  # merge control status to map
  map <- match_df %>% 
    select(id, is_control) %>% 
    distinct() %>%
    merge(map, ., all.x=TRUE)
  
  # indicate buffer zones
  map$is_control <- ifelse(map$id %in% buffer_zone, "Buffer Districts", map$is_control)
  
  # indicate places without cases in 2023
  if(length(cases_2023) > 0){
    map$is_control <- ifelse(! map$id %in% cases_2023, "No Cases in 2023, Excluded", map$is_control)
  }
  
  # indicate places that are not coastal
  if(length(coastal_dist) > 0){
    map$is_control <- ifelse(! map$id %in% coastal_dist, "Non-Coastal", map$is_control)
  }
  
  
  # generate and save plot
  ggplot()+
    geom_sf(data = map, aes(fill=is_control))+
    geom_sf(data = big_map, fill=NA, color="black", lwd=.8) + # bolds borders of big_map
    theme_void()+
    scale_fill_manual("",
                      breaks = c("Cyclone-affected", "Matched Control", "Cyclone-unaffected", "No Cases in 2023, Excluded", "Buffer District", "Non-coastal"),
                      values = c("#581845", "#6FC0DB", "#FFC300", "grey90", "grey50", "maroon"))
  ggsave(paste0("figs/", file_prefix, "-matchmap.png"), height=4, width=8, units="in")
}

#### TREATMENT EEFFECT PLOT ####
# Returns a plot of actual vs counterfactual cases and cyclone-attributable cases over time
#
# Inputs: case_df: a dataframe of case reports by week, year, and unit id
#         gsynth_out: gsynth object (can acess by running fect or calling the $gsynth_out entry from synth_fun)
#         cyclone_step: index when cyclone occurred (can be retrieved from synth_fun)
#         pop: full population size of the cyclone-affected districts 
#         pop.weights: population weights for the cyclone-affected districts (in the orde rof gsynth_out$tr)
#         file_prefix: prefix to save pdf image
#         years: the years over which climate data is provided (can be retrieved from match_fun)
#         year_ind: the indices for when a new year starts (can be retrieved from match_fun)
# Output: a pdf figure in the figs folder with suffix -att
att_plot <- function(case_df, gsynth_out, cyclone_step, 
                     pop, pop.weights, file_prefix,
                     year_ind, years, 
                     inc=TRUE){

  # identify treated indices
  treated_ind <- gsynth_out$D.dat %>% colSums() > 0
  
  # create dataframe of treatment effects
  att_df <- gsynth_out$est.att %>%
    data.frame() %>%
    mutate(t=row_number()) # will use this to convert time indices to dates
  
  # convert from incidence to cases 
    att_df %<>% 
    mutate(mid.num = mid.num*pop,
           CI.lower = CI.lower*pop,
           CI.upper = CI.upper*pop)

  
  
  att2 <-  att_df %>%
            ggplot() + 
            geom_ribbon(aes(x=t, ymin = CI.lower, ymax = CI.upper), fill="grey70")+ # 95% confidence interval
            geom_line(aes(x=t, y = mid.num))+
            scale_x_continuous(breaks=year_ind,  # convert to years x-axis
                               labels=years)+
            geom_hline(yintercept=0, color="maroon", linetype="dashed")+
            geom_vline(xintercept=cyclone_step, color="maroon", linetype="dashed")+  # indicate cyclone index
            xlab("Time (Years)")+
            ylab("Cyclone-Attributable Cases")+
            ggtitle("")+
            theme_classic()+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  text=element_text(size=14))
          
  ts_cases <- gsynth_out$Y.ct[,treated_ind] %>% # counterfactual cases in cyclone-affected districts
                apply(1, function(x) sum(x*pop.weights)*pop) %>% # convert from incidence to raw cases, sum across districts
                data.frame(cases=.) %>%
                mutate(time = row_number(),
                       group="Synthetic Control") %>% # label these as belonging to synthetic control
                rbind(data.frame(group="Observed", # actual cases in cyclone-affected units
                                 cases = gsynth_out$est.att$obs.cases) %>%
                        mutate(time = row_number()))
  
  
  att1 <- ts_cases %>%
            ggplot() +
            geom_line(aes(x=time, y = cases, color=group, linewidth=group))+
            ggtitle("")+
            scale_color_manual("", breaks=c("Synthetic Control", "Observed"),  # colors indicate observed vs synthetic control
                               values = c("red", "black"))+ 
            scale_linewidth_manual("", breaks=c("Synthetic Control", "Observed"),
                                   values = c(.2, .5))+  # different linewidth to increase visibility when overlapping
            scale_x_continuous(breaks=year_ind, # change x-axis to years
                               labels=years)+
            xlab('Time (Years)')+
            ylab("Dengue Cases")+
            geom_vline(xintercept=cyclone_step, color="maroon", linetype="dashed")+ # change x-axis to years
            theme_classic()+
            theme(legend.position="bottom")+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  text=element_text(size=14))
  
  plot_grid(plot_grid(att1+theme(legend.position="none"), att2, ncol=2, labels="AUTO"),
            plot_grid(get_legend(att1), "", ncol=2), nrow=2, rel_heights=c(9,1))
  ggsave(paste0("figs/", file_prefix, "-att.pdf"), height=4, width=8, units="in")
}


#### GET NUMBERS OF ATTRIBUTABLE CASES ####
# Returns numbers of attributable cases over a specified post-cyclone time period
# and prints key values
#
# Inputs: gsynth_out: gsynth object (can acess by running fect or calling the $gsynth_out entry from synth_fun)
#         case_df: a dataframe of case reports by week, year, and unit id
#         cyclone_step: index when cyclone occurred (can be retrieved from synth_fun)
#         tr_ind: optional, rownames (from gsynth_out$est.att) across which to sum treatment effect
#                 preset to timesteps when p < 0.05 for main analysis
# Output: a list of values calculated across the period specified by tr_ind, entries as follows
#         mid_inc: cyclone-attributable incidence
#         lower_inc: the lower estimate for 95% confidence interval on cyclone-attributable incidence
#         upper_inc: the upper estimate for 95% confidence interval on cyclone-attributable incidence
#         lower_pct: the lower estimate for 95% confidence interval on percent attributable cases
#         upper_pct: the upper estimate for 95% confidence interval on percent attributable cases
#         mid_pct: the percent of cases attributable to the cyclone
#         obs_inc: observed incidence
#         obs_cases: observed cases
#         mid_cases: the number of cyclone-attributable cases 
#         lower_cases: the lower estimate for 95% confidence interval on attributable cases
#         upper_cases: the uppe restimate for 95% confidence interval on attributable cases

att_print <- function(gsynth_out, case_df, cyclone_step,
                      pop_weights, pop,
                      tr_ind = c("3", "4", "5", "6", "7", "8", "9", "10")){
  
  # will use these to select cyclone-affected entries
  treated_ind <- gsynth_out$tr
  ntreated <- length(treated_ind)
  time_ind <- which(rownames(gsynth_out$est.att) %in% tr_ind)
  
  # across each bootstrap, calculate the proportion attributable cases during period indicated by tr_ind
  pct_quants <- sapply(1:length(gsynth_out$Y.boot), function(n) sum(gsynth_out$att.W.boot[time_ind,n], na.rm=TRUE)/(gsynth_out$Y.boot[[n]][time_ind, 1:ntreated] %>% colSums() %>% `*`(pop_weights[[n]]) %>% sum())) %>%
    quantile(c(.025, .975))
  
  # sum cyclone-attributable incidence over selected time period
  num_attr <- gsynth_out$est.att[time_ind,] %>%
    select(mid.num) %>%
    sum()
  
  # sum observed incidence over selected time period
  num_inc <- gsynth_out$est.att$obs.inc[time_ind] %>% sum()
  
  # create list of values
  vals <- list(mid_inc=num_attr,
               lower_inc=num_inc*as.numeric(pct_quants[1]),
               upper_inc=num_inc*as.numeric(pct_quants[2]),
               lower_pct=as.numeric(pct_quants[1]),
               upper_pct=as.numeric(pct_quants[2]),
               mid_pct=num_attr/num_inc,
               obs_inc=num_inc,
               obs_cases=num_inc*pop,
               mid_cases=num_attr*pop,
               lower_cases=lower_inc*pop,
               upper_cases=upper_inc*pop)
  
  # print the main result, including percent attributable cases
  print(paste0(round(vals$mid_cases), " (", round(vals$lower_cases), ", ", round(vals$upper_cases), ") cases were attributable to the cyclone, out of ", vals$obs_cases, " (", round(vals$mid_pct*100, digits=2), "%)"))
  
  return(vals)
}

#### SPATIAL TREATMENT EFFECT PLOT ####

# Returns a plot of the cyclone-attributable cases per thousand people in each treated district
# as both a map and time series
#
# Note that this script would need to be modified to apply to countries other than Peru
# because the facet labels are specified to match districts back to Peruvian regions
#
# Inputs: gsynth_out: gsynth object (can access by running fect or calling the $gsynth_out entry from synth_fun)
#         map: shapefile of spatial units at level used for analysis
#         big_map: shapefile of spatial units at higher administrative division to indicate borders
#         file_prefix: prefix to save pdf image
#         tr_ind: optional, rownames (from gsynth_out$est.att) across which to sum treatment effect
#                 preset to timesteps when p < 0.05 for main analysis
# Output: a pdf figure in the figs folder with suffix spatt-map

spatt_plot <- function(gsynth_out, map, big_map, file_prefix,
                       tr_ind=c("3", "4", "5", "6", "7", "8", "9", "10")){
  
  time_ind <- which(rownames(gsynth_out$est.att) %in% tr_ind)
  
  # create a dataframe with columns att (# attributable cases) and id
  # summed across period specified by tr_ind in treated districts
  att_pc_df <- gsynth_out$eff[time_ind, gsynth_out$tr] %>%
    colSums(na.rm=TRUE) %>%
    data.frame(att = .,
               id = gsynth_out$id[gsynth_out$tr]) 
    mutate(att_pc = att * 1000) 
  
  spatt_map <- merge(map, att_pc_df) %>% # append to map of districts
    rename(`Attributable Cases \n(per Thousand)` = att_pc)  %>%
    ggplot()+
    geom_sf(aes(fill=`Attributable Cases \n(per Thousand)`), color="black", lwd=.3)+
    geom_sf(data=big_map, fill=NA, color="black", lwd=.8)+ # add big_map borders
    scale_fill_stepsn(colours=c("#7E64C5", "#FFFFFF", "#FF7F7F", "#FF0000", "#BA0007", "#75000E", "#5B000B", "#420008", "#73000E", "black"),
                      breaks=c(-5, 0, 5, 10, 15, 20, 25, 30),
                      labels=c("", 0, "", 10, "", 20, "", ">30"))+ #colors specified for main analysis, may wish to modify breaks for a different mode
    ylim(c(-9, -3))+
    xlim(c(-82, -77))+
    theme_void()+
    theme(legend.position="bottom")
  
  # begin processing for the time series plots
  spatt_df <-  gsynth_out$eff[,gsynth_out$tr] %>% 
    tail(10) %>% # go from march through the end of 2023
    data.frame() %>%
    mutate(step = row_number()) # will use this column to convert to dates
  
  
  spatt_df <-  gsynth_out$eff[,gsynth_out$tr] %>% 
    tail(10) %>%  # go from march through the end of 2023
    data.frame() %>%
    mutate(step = row_number()) # will use this column to convert to dates
  
  colnames(spatt_df) <- c(gsynth_out$id[gsynth_out$tr], "step")
  
  # convert to date indices (these will be last date of four-week period across which attributable cases were calculated)
  epiweek_df <- data.frame(dates= seq.Date(from = as_date("2023-03-25"),
                                           to = as_date("2023-12-29"),
                                           by = "28 days"),
                           step=1:10) 
  
  # will use this to group districts by region
  dept_name <-  read_xlsx("maps/UBIGEODISTRITOS.XLSX") %>%
                  rename(id = IDDIST,
                         departamento = NOMBDEP) %>%
                  select(id, departamento)  
  
  # regions that include at least one treated district, ordered to correspond with geographic position when faceted
  dept_name$departamento <- factor(dept_name$departamento, levels=c("TUMBES", "CAJAMARCA", "PIURA", "SAN MARTIN", "LAMBAYEQUE", "LA LIBERTAD"))
  
  spatt_scatter <- spatt_df %>% 
    left_join(epiweek_df) %>%
    select(-step) %>%
    pivot_longer(!dates, # pivot so we can plot each district as its own line
                 names_to = "id", 
                 values_to = "TE")  %>%
    left_join(dept_name) %>%
    mutate(TE_pc = TE * 1000) %>% # calculate attributable cases per thousand people
    ggplot(aes(y=TE_pc, x=dates, group=id)) +
    geom_line(linewidth=.2, alpha=.5)+
    theme_classic()+
    facet_wrap(~departamento, ncol=2, scales="free")+ # gives axes to all plots
    ylab("Attributable Cases (per Thousand)")+
    xlab("Time")+
    theme(legend.position="none")+
    scale_x_date(date_breaks = "2 months", date_labels = "%b")+ # make the x-axis (time) nice
    geom_hline(yintercept=0, color="maroon", linetype="dashed")+ # indicate zero attributable cases
    scale_y_continuous(limits=c(-10, 50)) # may need to change this if not plotting main analysis
  
  plot_grid(spatt_scatter)
  ggsave(paste0("figs/", file_prefix, "-spatt-scatter.pdf"), height=6, width=8, units="in", dpi=700)
  
  plot_grid(spatt_map, spatt_scatter, ncol = 2, labels="AUTO") # combine both plots into a grid
  
  ggsave(paste0("figs/", file_prefix, "-spatt-map.png"), height=6, width=8, units="in", dpi=700)
  
  
}

#### MATCHING FUNCTION ####

# Matches treated districts to Cyclone-unaffected districts with most similar climate conditions
# and generates tables and (optional) plots related to this process
#
# Inputs: anomaly_upper: upper threshold for precipitation anomaly (above this considered treated)
#         anomaly_lower: lower threshold for precipitation anomaly (below this considered treated)
#                         anomaly lower must be less or equal to anomaly upper
#         coastal_dist:  optional, vector of coastal units if analysis should only be conducted on them
#         cases_2023:   optional, vector of units that reported cases in 2023 (other units excluded)
#         my_country:   countries to include in analysis. PER3 is Peru at admin level 3 (district)
#                       use this for region-level analysis: c("PER", "COL1", "ECU1", "MEX", "BRA1")
#         match_num: how many Cyclone-unaffected units each treated district should be matched to
#         plot_bal: TRUE/FALSE, whether to plot standardized difference and mean values over time for climate covariates (to visualize balance
#         plot_map: TRUE/FALSE, whether to plot map of which units were treated, Cyclone-unaffected, matched control, excluded
#         file_prefix: prefix to save pdf image (if plot_bal or plot_map is true)
#         map: shapefile of spatial units at level used for analysis (if plot_map is true)
#         big_map: shapefile of spatial units at higher administrative division to indicate borders (if plot_map is true)
#         treated_names: leave empty to use the default defined by anomaly_upper, otherwise override and set your own treated districts
# Output: a pdf figures in the figs folder if requested and a list with the following entries
#         table: a table describing mean values across cyclone-unaffected, cyclone-affected, and control groups
#         match_names: names of the units included in matched control pool
#         df: climate covariates over time and designation (treated, Cyclone-unaffected, control) for each district
#         treated_names: names of treated units
#         match_obj: output of the PanelMatch package
#         balance: output of get_covariate_balance, gives standardized difference over time with respect to climate covariates
#         abs_bal: output of get_covariate balance, gives absolute standardized difference over time with respect to climate covariates
#         match_list: a list of length treated_names where each entry is the units matched to a given treated units
#         years: the years over which climate data is provided 
#         year_ind: the indices for when a new year starts 
#         cyclone_step: the index when the cyclone occured
match_fun <- function(anomaly_upper = .0085, anomaly_lower = .007,
                      coastal_dist = c(), cases_2023 = c(),
                      my_country = "PER3", 
                      match_num = 10, 
                      plot_bal=FALSE, plot_map=FALSE, file_prefix=NA,
                      map=NULL, big_map=NULL,
                      treated_names=c()){
  
  # Return error if file prefix/maps are needed but not provided
  if((plot_bal==TRUE | plot_map==TRUE) & is.na(file_prefix)){
    stop("File prefix required to plot")
  }
  
  
  if((plot_map==TRUE) & (is.null(map) | is.null(big_map))){
    stop("Provide map to plot")
  }
    
  # identify treated, Cyclone-unaffected, and buffer units in Peru based on upper/lower anomaly threshold
  anomaly_df <- read.csv("anomaly_df.csv")
  
  anomaly_df %>% filter(diff_rain>anomaly_upper & country %in% c("PER3", "PER")) %>% select(id) %>% unique() %>% unlist() -> extreme_ids
  anomaly_df %>% filter(diff_rain<=anomaly_lower) %>% select(id) %>% unique() %>% unlist() -> nonextreme_ids
  anomaly_df %>% filter(diff_rain<anomaly_upper & diff_rain>=anomaly_lower) %>% select(id) %>% unique() %>% unlist() -> buffer_ids  
  
  
  # override cyclone-affected districts if manually provided 
  if(length(treated_names)>0){
    extreme_ids <- treated_names 
  }
    
  # read in climate dataframe for matching
  clim_df <- read.csv("clim_df.csv") 
  
  # if coastal_dist and cases_2023 aren't specified, choose everywhere
  if(length(coastal_dist)==0){
    coastal_dist_set <- unique(clim_df$id)
  } else{
    coastal_dist_set <- coastal_dist
  }
  
  if(length(cases_2023)==0){
    cases_2023_set <- unique(clim_df$id)
  } else{
    cases_2023_set <- cases_2023
  }
  
  clim_df %<>%
    # focus on the requested spatial units
    filter(country %in% my_country) %>%
    filter(!is.na(rain)) %>%
    filter(id %in% coastal_dist_set) %>%
    filter(id %in% cases_2023_set) %>%
    filter(id %in% c(extreme_ids, nonextreme_ids)) %>%
    # indicate post_cyclone observations
    mutate(date = as_date(date)) %>%
    mutate(int = ifelse(date >= as_date("2023-03-07") & id %in% extreme_ids, 1, 0)) %>%
    mutate(week = epiweek(date), year = year(date)) %>%
    filter(year >= 2018) # match from 2018 onward
  
  # define 4-week periods ("steps")
  step_help <- clim_df %>%
    group_by(week, year) %>%
    dplyr::summarize(date_end = max(date)) %>%
    ungroup() %>%
    arrange(date_end) %>%
    mutate(weeknum = row_number()) %>%
    mutate(step = ceiling(weeknum/4)) %>%
    mutate(step = as.integer(step))
  
  # this will help with plotting - identify steps when new years begin 
  years <- unique(step_help$year)
  year_ind <- sapply(unique(step_help$year), function(x) min(which(step_help$year==x))/4)
  
  # will use this to filter out if there's a step with fewer than four weeks
  step_filter <- step_help %>%
    group_by(step) %>%
    dplyr::summarize(frq = n()) %>%
    filter(frq < 4) %>%
    select(step) %>%
    unlist()
  
  # take averages across four-week periods
  clim_df %<>% left_join(step_help) %>% 
    filter(! step %in% step_filter) %>%
    group_by(id, country, step) %>%
    dplyr::summarize(mean_rel_r0 = mean(rel_r0),
                     mean_rain = mean(rain),
                     mean_temp = mean(temp),
                     int = max(int),
                     date_end = max(date)) %>%
    mutate(mean_rain = mean_rain * 1000) %>%
    # format for matching package
    transform(place = as.integer(as.factor(id))) %>%
    mutate(is_control = ifelse(id %in% extreme_ids, "Cyclone-affected", "Cyclone-unaffected"))
  
  # how many steps to match on
  lag_num <- nrow(filter(clim_df, is_control == "Cyclone-affected" & int==0) %>%
                    select(step) %>%
                    distinct())
  
  cyclone_step <- lag_num+1
  
  # do not conduct any matching, just compare cyclone-affected vs Cyclone-unaffected units
  unmatch_res <- PanelMatch(lag = lag_num, time.id = "step", unit.id = "place",
                            treatment = "int", refinement.method = "none",
                            data = clim_df, match.missing =FALSE,
                            size.match = match_num, qoi = "att", 
                            use.diagonal.variance.matrix = TRUE, 
                            covs.formula = ~ mean_temp + mean_rain,
                            outcome.var = "mean_rain")
  
  # conduct matching for each of the cyclone-affected units
  match_res <- PanelMatch(lag = lag_num, time.id = "step", unit.id = "place",
                          treatment = "int", refinement.method = "mahalanobis",
                          data = clim_df, match.missing =FALSE,
                          size.match = match_num, qoi = "att", 
                          use.diagonal.variance.matrix = TRUE, 
                          covs.formula = ~ mean_temp + mean_rain,
                          outcome.var = "mean_rain") 
  
  # calculate standardized differences
  bal_calc <- get_covariate_balance(match_res$att, clim_df, c("mean_rain", "mean_temp"), plot=FALSE) %>% colMeans()
  
  # absolute standardized differences
  abs_bal_calc <- get_covariate_balance(match_res$att, clim_df, c("mean_rain", "mean_temp"), plot=FALSE) %>% abs() %>% colMeans()
  
  # extract the names of matched units
  matched_prov <- list()
  
  # how many times was each matched unit selected (define weights to take weighted average)
  for(i in 1:length(match_res$att)){
    matched_prov[[i]] <- names(attr(match_res$att[[i]], "weights")[attr(match_res$att[[i]], "weights") != 0]) 
  }
  
  # add the weights to the dataframe for matched control units
  match_df <- matched_prov %>% 
    unlist() %>% 
    as.numeric() %>%
    data.frame(place = .) %>% 
    group_by(place) %>% 
    dplyr::summarize(weight=n()) %>%
    right_join(clim_df) %>%
    mutate(weight = ifelse(is.na(weight), 1, weight)) # if not in matched control, everything is weighted equally
  
  # get names of the matched provinces
  matched_names <- match_df %>% filter(place %in% unlist(matched_prov)) %>% select(id) %>% unlist() %>% unique()
  
  # label which units are in the matched control
  match_df %<>% mutate(is_control = ifelse(id %in% matched_names, "Matched Control", is_control)) 
  
  # table comparing covariate means in treated, Cyclone-unaffected, and matched control
  match_tab <-   match_df %>%
    filter(is_control == "Matched Control") %>%
    mutate(is_control = "Cyclone-unaffected",
           weight = 1) %>%
    rbind(match_df) %>% # we need to also have the matched control units counted in the Cyclone-unaffected pool
    group_by(is_control) %>%
    # weighted averages and standard deviations
    dplyr::summarize(`Mean Temp.` = paste0(mean(mean_temp), " (", weighted.mean(mean_temp, weight), ")"), 
                     `Mean Precip.` = paste0(mean(mean_rain), " (", weighted.mean(mean_rain, weight), ")"),
                     `Sd. Temp.` = paste0(sd(mean_temp), " (", sqrt(Hmisc::wtd.var(mean_temp, weight)), ")"),
                     `Sd. Precip.` = paste0(sd(mean_rain), " (", sqrt(Hmisc::wtd.var(mean_rain, weight)), ")"),
                     n = length(unique(id)))  
  
  # names of treated units
  treated_names <- filter(match_df, is_control == "Cyclone-affected") %>%
    select(id) %>% 
    unlist() %>%
    unique()
  
  # values that the function will return
  match_out <- list("table" = match_tab,
                    "match_names" = matched_names, 
                    "df" = match_df,
                    "treated_names" = treated_names,
                    "match_obj" = match_res,
                    "balance" = bal_calc,
                    "abs_bal" = abs_bal_calc,
                    "match_list" = matched_prov,
                    "years" = years,
                    "year_ind" = year_ind,
                    "cyclone_step" = cyclone_step)
  
  # balance plots if desired
  if(plot_bal == TRUE){
    balance_plot(match_out, cyclone_step, file_prefix, years, year_ind)
    climts_plot(match_out, cyclone_step, file_prefix, years, year_ind)
  }
  
  # map plot if desired
  if(plot_map == TRUE){
    matchmap_plot(map, big_map, file_prefix, match_out,
                  cases_2023 = cases_2023, buffer_zone=buffer_ids, coastal_dist = coastal_dist)
  }
  
  return(match_out)
}

#### GENERALIZED SYNTHETIC CONTROL FUNCTION ####

# Conducts generalized synthetic control analysis and returns values, optional plots
#
# Inputs: case_df: a dataframe of case reports by week, year, and unit id
#         match_out: output of the match_fun function
#         pop_df: a dataframe of population sizes by id and year
#         file_prefix: prefix to save pdf image (if spatt_plot or att_plot is true)
#         att_plot: TRUE/FALSE, whether to attributable cases over time 
#         spatt_plot: TRUE/FALSE, whether to plot map of attributable cases by unit
#         map: shapefile of spatial units at level used for analysis (if spatt_plot is true)
#         big_map: shapefile of spatial units at higher administrative division to indicate borders (if spatt_plot is true)
#         lf_num: specify the number of latent factors to use (bypasses cross-validation for optimal latent factor number)
#         r.max: the maximum number of latent factors to test out in cross-validation for number of latent factors
#         use_clim: TRUE/FALSE, whether to include climate covariates in the synthetic control model
#         use_r0: TRUE/FALSE, whether to use temperature-dependent R0 instead of mean temperature in synthetic control model
#         inc: TRUE/FALSE, whether to conduct analysis on incidence instead of absolute cases. Set to false at own risk, other functions not adapted for this output.
#         start_year: earliest year to include in analysis
#         exclude_year: vector of years to be excluded from analysis
#         end_week: last week in 2023 to include in analysis
#         log_cases: TRUE/FALSE, whether to conduct analysis on logged cases. Use at own risk, other functions not adapted for this output. 
#                     log-scaling not recommended for fect, see 
#         method: specifies type of inference to conduct in fect package. default to ife (resampling to bootstrap CI). could use gsynth (conformal prediction interval).

# Output: pdf figures in the figs folder if requested and a list with the following entries
#         gsynth_obj: a gsynth object from the fect package
#         att_nums: summary of attributable cases (from  function)
#         years: the years over which climate data is provided 
#         year_ind: the indices for when a new year starts 
#         cyclone_step: index when cyclone occurred 
#         pop: full population size of the cyclone-affected districts 
#         pop.weights: population weights for the cyclone-affected districts (in the order of gsynth_out$tr)
synth_fun <- function(case_df, match_out, pop_df, file_prefix, 
                      att_plot = FALSE,
                      spatt_plot = FALSE, map = NULL,
                      big_map = NULL, 
                      lf_num=NA, r.max=5, use_clim=TRUE,
                      use_r0 = FALSE,
                      inc=TRUE, start_year = 2010,
                      exclude_year = c(),
                      end_week = 52, log_cases=FALSE,
                      method="ife") {
  
  # fail if requesting a map without providing shapefile
  if(spatt_plot==TRUE & is.null(map)){
    stop("Provide map to plot")
  }
  
  # treated and matched units
  incl_units <- c(match_out$treated_names, match_out$match_names)
  
  if(use_clim == TRUE){
    # read in climate
    df <- fread("clim_df.csv") %>%
      # filter to treated and matched control regions
      filter(id %in% incl_units) %>%
      filter(date <= as_date("2023-12-31")) %>%
      # convert to epiweek
      mutate(week=epiweek(date), year=year(date), month=month(date))  %>%
      # sometimes the last few days of a year get assigned to first epiweek of following year
      mutate(year = ifelse(week == 1 & month == 12, year+1, year)) %>%
      group_by(week, year, id) %>%
      dplyr::summarize(mean_temp = mean(temp),
                       mean_rain = mean(rain),
                       mean_rel_r0 = mean(rel_r0)) %>%
      left_join(case_df) %>%
      filter(!is.na(mean_temp)) %>%
      filter(id %in% incl_units) %>%
      # filter to specified time period
      filter(year >= start_year) %>%
      filter(! year %in% exclude_year) %>%
      filter(week <= end_week) %>%
      filter(year <= 2023)
  } else{
    # still read in the climate covariates even though we won't use them to make sure the same districts are studied
    df <- fread("clim_df.csv") %>%
      # filter to treated and matched control regions
      filter(id %in% incl_units) %>%
      filter(date <= as_date("2023-12-31")) %>%
      # convert to epiweek
      mutate(week=epiweek(date), year=year(date), month=month(date))  %>%
      # sometimes the last few days of a year get assigned to first epiweek of following year
      mutate(year = ifelse(week == 1 & month == 12, year+1, year)) %>%
      group_by(week, year, id) %>%
      dplyr::summarize(mean_temp = mean(temp),
                       mean_rain = mean(rain),
                       mean_rel_r0 = mean(rel_r0)) %>%
      left_join(case_df) %>%
      filter(!is.na(mean_temp)) %>%
      # don't actually need the climate covariates
      mutate(mean_temp = NA,
             mean_rain = NA,
             mean_rel_r0 = NA) %>%
      filter(id %in% incl_units) %>%
      filter(year >= start_year) %>%
      filter(! year %in% exclude_year) %>%
      filter(week <= end_week) %>%
      filter(year <= 2023)
  }
  
  # we'll use this to group into 4-week periods
  time_df <- df %>%
    mutate(time = week+(year-min(case_df$year))*100) %>%
    select(time, week, year) %>%
    distinct() %>%
    arrange(time) %>%
    ungroup() %>%
    mutate(weeknum = row_number()) %>%
    mutate(step = ceiling(weeknum/4)) %>%
    mutate(step = as.integer(step))
  
  # will use this in plotting later
  year_ind <- sapply(unique(time_df$year), function(x) min(which(time_df$year==x))/4)
  years <- unique(time_df$year)
  
  # aggregate to 4-epiweek periods, take average within those
  df <- time_df %>%
    right_join(df) %>%
    group_by(id, step) %>%
    dplyr::summarize(cases = sum(cases),
                     mean_temp = mean(mean_temp),
                     mean_rel_r0 = mean(mean_rel_r0),
                     mean_rain = mean(mean_rain)*1000,
                     year = max(year)) 
  
  # when did the cyclone happen?  
  cyclone_step <- unique(time_df$step[time_df$week == 10 & time_df$year==2023])
  
  # input a zero in weeks with missing data
  df <- expand.grid(step=1:max(df$step, na.rm=TRUE), id=incl_units) %>%
    left_join(df) %>%
    left_join(pop_df) %>%
    mutate(inc = cases/pop) %>%
    mutate(cases = ifelse(is.na(cases), 0, cases),
           inc = ifelse(is.na(inc) | is.infinite(inc), 0, inc)) %>%
    mutate(int = ifelse(id %in% match_out$treated_names & step >= cyclone_step, 1, 0),
           weight = 1) %>%
    left_join(., match_out$df %>% select(id, is_control) %>% distinct() %>% filter(is_control %in% c("Cyclone-affected", "Matched Control"))) 
  
  tr_pop <- df %>% filter(int==1 & year==2023) %>% select(pop, id) %>% distinct() %>% filter(!is.na(pop)) %>% select(pop) %>% sum()
  
  #  if specified, use incidence or logged cases as outcome
  if(inc == TRUE){
    df %<>% mutate(y = inc,
                   weight = pop) 
  }
  else if(log_cases == TRUE){
    df %<>% mutate(y = log(cases+exp(-1)))
  }
  else{
    df %<>% mutate(y = cases)
  }
  
  # whether to use cross-validation period or pre-set the number of latent factors
  if(is.na(lf_num)){
    CV <- TRUE
    r <- c(0,r.max)
  }
  
  else{
    CV <- FALSE
    r <- lf_num
  }
  
  # lets us get district-level estimates more easily 
  df$group <- df$id
  
  # running the fect package to get the synthetic control results
  if(use_clim == TRUE){
    if(use_r0 == TRUE){
      gsynth_out <- df %>%
        fect(y ~ int + mean_rel_r0 + mean_rain, data = ., 
             index=c("id", "step"), 
             force = "two-way", 
             method=method,
             se = TRUE, 
             CV = CV, r = r,
             seed = 514,
             nboots = 1000,
             group="group",
             W="weight")
  # model without climate covariates
    } else {
      gsynth_out <- df %>%
        fect(y ~ int + mean_temp + mean_rain, data = ., 
             index=c("id", "step"), 
             force = "two-way", 
             method=method,
             se = TRUE, 
             CV = CV, r = r,
             seed = 514,
             nboots = 1000,
             group="group",
             W="weight")
    }
    
  } else {
    gsynth_out <- df %>%
      fect(y ~ int, data = ., 
           index=c("id", "step"), 
           force = "two-way", 
           method=method,
           se = TRUE, 
           CV = CV, r = r,
           seed = 514,
           nboots=1000,
           group="group",
           W="weight")
  }
  
  # get population weights for bootstraps
  if(inc == TRUE){
    # which treated locations are included in each bootstrap?
    boot.locs <- lapply(gsynth_out$boot.ids, function(n) gsynth_out$id[n[1:length(gsynth_out$tr)]])
    # what are their population sizes
    boot.pops <- lapply(boot.locs, function(boot) unlist(sapply(boot, function(loc) pop_df$pop[which(pop_df$id == loc & pop_df$year == 2023)])))
    # population weights for each bootstrap
    boot.weights <- lapply(boot.pops, function(each.pop) each.pop/sum(each.pop))
    
    act.locs <- gsynth_out$id[gsynth_out$tr]
    act.pops <- sapply(act.locs, function(loc) pop_df$pop[which(pop_df$id == loc & pop_df$year == 2023)])
    act.weights <- unlist(act.pops)/tr_pop
  }
  
  else{
    boot.weights <- lapply(1:length(gsynth_out$boot.ids), function(n) rep(1, length.out = length(gsynth_out$tr)))
  }
  
  # calculate percent attributable cases based on bootstraps
  lapply(1:length(gsynth_out$Y.boot), function(n) apply(gsynth_out$Y.boot[[n]][,1:length(gsynth_out$tr)], 1, function(x) sum(x*boot.weights[[n]]))) %>%
    do.call(cbind, .) %>%
    `/`(gsynth_out$att.W.boot, .) %>% ## check this
    ifelse(is.infinite(.), 0, .) %>% # set proportion to zero when no reported cases
    apply(1, function(x) c(quantile(x, c(.025, .975)), sd(x)/sqrt(length(x)), sum(x<=0)/length(x))) %>% # recalculate 95% CI, standard error, p-value
    t() %>%
    data.frame() %>%
    rename(lower.pct = 1,
           upper.pct = 2,
           recalc.SE = 3,
           recalc.p = 4
    ) %>%
    cbind(obs.inc = apply(gsynth_out$Y.dat[,gsynth_out$tr], 1, function(x) sum(x * act.weights))) %>%
    mutate(obs.cases = obs.inc*tr_pop) %>%
    mutate(lower.num = lower.pct*obs.inc,  #use reported cases and percent attributable to calculate number attributable
           upper.num=upper.pct*obs.inc) %>%
    cbind(gsynth_out$est.att.W) %>%
    mutate(mid.pct = ATT/obs.inc,
           mid.num=ATT,
           mid.cases=ATT*tr_pop,
           upper.cases=upper.num*tr_pop,
           lower.cases=lower.num*tr_pop) -> gsynth_out$est.att
  
  # calculate rsq
  gsynth_out$rsq <- cor(c(gsynth_out$Y.dat[1:length(gsynth_out$pre.periods),gsynth_out$tr]), 
                        c(gsynth_out$Y.ct[1:length(gsynth_out$pre.periods),gsynth_out$tr]))^2
  
  # generate plots if requested
  if(att_plot==TRUE){
    att_plot(df, gsynth_out, cyclone_step, tr_pop, act.weights, file_prefix,
             year_ind, unique(time_df$year), inc = inc)
  }
  
  if(spatt_plot==TRUE){
    spatt_plot(gsynth_out,  
               map = map, big_map = big_map, 
               file_prefix=file_prefix)
  }
  
  # get attributable numbers
  att_nums <- att_print(gsynth_out, df, cyclone_step,
                        boot.weights, tr_pop)
  
  return(list("gsynth_obj"=gsynth_out,
              "att_nums"=att_nums,
              "year_ind"=year_ind,
              "years"=years,
              "cyclone_step"=cyclone_step,
              "pop"=tr_pop, 
              "pop.weights"=act.weights))
}

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

# extract cyclone-attributable incidence in each district across the post-cyclone period (April 22 - Nov 3)
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

# for each bootstrap, refit the model and store the coefficients 
# here, we're resampling across both cyclone-affected districts and their bootstrapped estimates
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

# plot the relationship between the RCs and cyclone-attributable incidence
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
  ylab("Cyclone-attributable \ncases per thousand")+
  xlab("Factor")+
  theme(text=element_text(size=10))

# plot the temperature vs cyclone-attributable cases
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
  ylab("Cyclone-attributable \ncases per thousand")+
  theme(text=element_text(size=10))

plot_grid(p1, p2, nrow=2, labels="AUTO")
ggsave(paste0("figs/vul-ind.pdf"), height=11, width=11, units="cm")
