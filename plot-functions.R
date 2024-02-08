plot_synth <- function(microsynth_out, var, transform_df=data.frame(), int_date = as.Date("2023-03-07"),  div = 5, var_name = "", legend.pos = "bottom", rescale_df = data.frame()){
  var_name <- ifelse(var_name == "", var, var_name)
  time <- as.numeric(names(microsynth_out$Plot.Stats$Treat[var,])) %>% as.Date(origin = "1970-01-01")
  treatment <- data.frame(val = microsynth_out$Plot.Stats$Treatment[var,], type="Treatment", Time = time)
  control <- data.frame(val = microsynth_out$Plot.Stats$Control[var,], type="Control", Time = time)
  all <- data.frame(val = microsynth_out$Plot.Stats$All[var,]*microsynth_out$Plot.Stats$scale.by, type="All Untreated", Time = time)
  
  synth_results <- rbind(treatment, control, all)

  
  if(sum(dim(transform_df))==0){
    mult <- 1
  } else {
    mult <- transform_df[transform_df$var==var, "sd"]
  }
  
  synth_results %>% 
    mutate(val = val/div*mult) %>%
  ggplot() +
    geom_line(aes(y=val, x=Time, group=type, colour=type))+ # 12 treated units * period of 4
    theme_classic()+
    theme(legend.position = legend.pos)+
    ylab(var_name)+
    scale_colour_manual("", values = c("Treatment" = "black", "Control" = "maroon", "All Untreated" = "grey"))+
    geom_vline(xintercept = int_date, lty="dashed", color="red")
}

plot_diff <- function(microsynth_out, var, int_date = as.Date("2023-03-07"),  var_name = "", placebo=FALSE, rescale_df = data.frame()){
  var_name <- ifelse(var_name == "", var, var_name)
  time <- as.numeric(names(microsynth_out$Plot.Stats$Treat[var,])) %>% as.Date(origin = "1970-01-01")
  diff <- microsynth_out$Plot.Stats$Difference[var, 1,]
  diff_results <- data.frame("Time" = time, "Diff" = diff) %>%
    mutate(treated = ifelse(time >= int_date, TRUE, FALSE)) 
              
  if(nrow(rescale_df)>0){
    scale <- as.numeric(rescale_df[1, paste0("sd.", var)])
    diff_results$Diff <- scale * diff_results$Diff
  }
  
  plot <- ggplot() +
            geom_line(data=diff_results, aes(y=Diff/5, x=Time))+
            theme_classic()+
            theme(legend.position = "none")+
            ylab(var_name)+
            geom_vline(xintercept = int_date, lty="dashed", color="red")+
            ylab(paste0(var_name, '\n(Treated - Control)'))+
            geom_hline(yintercept = 0, lty="dashed", color = "red")
  
  if(placebo==TRUE){
    placebo <- microsynth_out$Plot.Stats$Difference[var,-1,] %>%
      t() %>%
      data.frame() %>%
      rownames_to_column("Time") %>%
      pivot_longer(cols=starts_with("Perm"), values_to = "Diff") %>%
      mutate(Time = as.Date(as.numeric(Time), origin = "1970-01-01"))
    
    if(nrow(rescale_df)>0){
      placebo$Diff <- scale * placebo$Diff
    }
    
    plot$layers <- c(geom_line(data=placebo, aes(y=Diff/5, x=Time, group=name), color="gray")
, plot$layers)
    
    plot
      
  }
  else{
    plot
  }
}


plot_multidiff <- function(microsynth_out, var, treated, int_date = as.Date("2023-03-07"),  var_name = ""){
  var_name <- ifelse(var_name == "", var, var_name)
  times <- as.numeric(names(microsynth_out[[1]]$Plot.Stats$Treat[var,])) 
  diff_results <- sapply(microsynth_out, function(x) x$Plot.Stats$Difference[var, 1,]) %>%
          cbind("Time" = times) %>%
          data.frame() %>%
          pivot_longer(! Time, names_to = "id", values_to = "Diff") %>%
          mutate(treated = ifelse(id %in% treated, TRUE, FALSE)) %>%
          mutate(Time = as.Date(Time, origin = "1970-01-01"))
  
  ggplot() +
    geom_line(data=diff_results %>% filter(treated == FALSE), aes(y=Diff, x=Time, group=id), colour="grey80")+
    geom_line(data=diff_results %>% filter(treated == TRUE), aes(y=Diff, x=Time, group=id, color=id))+
    theme_classic()+
    theme(legend.position = "none")+
    ylab(var_name)+
    geom_vline(xintercept = int_date, lty="dashed", color="red")+
    ylab(paste0(var_name, '\n(Treated - Control)'))+
    geom_hline(yintercept = 0, lty="dashed", color = "red")+
    theme(legend.position="right")+
    scale_color_discrete("Province", breaks=c("EC13", "EC24", "PE24", "PE20", "PE14"))
   #geom_text(data = diff_results %>% filter(treated == TRUE, Time == max(times), id != "PE14"),
    #          aes(y = Diff, x = Time, label = id), hjust = "left", size=3.4)+
    #annotate("text", y=2.1, x = max(diff_results$Time), label="PE14", hjust = "left", size=3.4)+
    #expand_limits(x = as.Date(c("2014-01-01", "2023-12-01")))
  

}

hist_multidiff <- function(microsynth_out, var, treated, int_date = as.Date("2023-03-07"),  var_name = ""){
  times <- as.numeric(names(microsynth_out[[1]]$Plot.Stats$Treat[var,])) 
  after_times <- which(times >= int_date)
  
  atts <- sapply(microsynth_out, function(x) sum(exp(x$Plot.Stats$Treat[var, after_times])-1) / sum(exp(x$Plot.Stats$Control[var, after_times])-1)) %>%
            data.frame(att = ., id = names(microsynth_out)) %>%
            mutate(treated = ifelse(id %in% treated, TRUE, FALSE))
  
  p <- ggplot() +  
    geom_histogram(data = atts %>% filter(treated == FALSE), aes(x = att)) +
    geom_segment(data = atts %>% filter(treated == TRUE), aes(x = att, xend = att, y=0, yend=11), colour="blue")+
    scale_x_log10()+
    scale_y_continuous(limits=c(0, 15))+ 
    theme_classic()+
    geom_text(data = atts %>% filter(treated == TRUE), aes(x = att, label = id), y = 12.5, angle=90)

  
  print(p)
}

tab_multidiff <- function(microsynth_out, var, treated, int_date = as.Date("2023-03-07")){
  times <- as.numeric(names(microsynth_out[[1]]$Plot.Stats$Treat[var,])) 
  after_times <- which(times >= int_date)
  
  att_df <- sapply(microsynth_out, function(x) sum(exp(x$Plot.Stats$Treat[var, after_times])-1) / sum(exp(x$Plot.Stats$Control[var, after_times]))-1) %>%
    data.frame(att = ., id = names(microsynth_out)) %>%
    mutate(treated = ifelse(id %in% treated, TRUE, FALSE))
  
  atts <- c()
  pvals <- c()
  for(prov in treated){
    my_att <- filter(att_df, id == prov) %>% select(att) %>% as.numeric()
    n_exceed <- att_df %>% 
      filter(treated == FALSE) %>% 
      select(att) %>% 
      mutate(exceeds = att >= my_att) %>%
      select(exceeds) %>%
      unlist() %>%
      sum() 
    my_pval <- n_exceed / nrow(att_df %>% filter(treated == FALSE))
    
    atts <- c(atts, my_att)
    pvals <- c(pvals, my_pval)
  }
  
  return(list(ADM1_PCODE = treated, atts = atts, pvals=pvals))
  
}

plot_diff_explainer <- function(microsynth_out, var, int_date = as.Date("2023-03-07"),  var_name = "", perm_num=1, cols=rev(rainbow(10)))
{
  var_name <- ifelse(var_name == "", var, var_name)
  time <- as.numeric(names(microsynth_out$Plot.Stats$Treat[var,])) %>% as.Date(origin = "1970-01-01")
  
  diff <- microsynth_out$Plot.Stats$Difference[var, 2:(perm_num+1),] %>%
            data.frame() 
  
  if(perm_num > 1){
    diff %<>% t()
  }
  else{
    names(diff) <- "Perm1"
  }
  
  diff_results <- diff %>% 
                   cbind("Time" = time) %>%
                   data.frame() %>%
                   pivot_longer(cols=starts_with("Perm"), values_to = "Diff") %>%
                   mutate(Time = Time %>% as.Date(origin = "1970-01-01"))
                        
  
  plot <- ggplot() +
    geom_line(data=diff_results, aes(y=Diff/5, x=Time, color=name))+
    theme_classic()+
    theme(legend.position = "none")+
    ylab(var_name)+
    geom_vline(xintercept = int_date, lty="dashed", color="black")+
    ylab(paste0(var_name, ' (Treated - Control)'))+
    geom_hline(yintercept = 0, lty="dashed", color = "black")+
    scale_color_manual(values=cols[1:perm_num])+
    scale_y_continuous(limits=c(-1.2, 1.2))+
    scale_x_date(date_labels = "%Y")
  
  plot
  
}

plot_weights_peru <- function(microsynth_out, map){
  missing_prov <- data.frame(name = setdiff(map$name, toupper(microsynth_out$w$Weights %>% rownames)),
                             weights=0, 
                             Weighting = NA)
  
  colfunc <- viridis(5, option="G")
  
  data.frame(weights = microsynth_out$w$Weights[,1],
             id = toupper(rownames(microsynth_out$w$Weights))) %>%
    mutate(break_weights = cut(weights, breaks = 10, include.lowest=FALSE)) %>%
    mutate(break_weights = as.character(break_weights)) %>%
    #mutate(break_weights = ifelse(weights==0, "0", break_weights)) %>%
    mutate(break_weights = ifelse(weights==1, "Treated", break_weights)) %>%  
    rename(name = id,
           Weighting = break_weights) %>%
    rbind(missing_prov) %>%
    merge(map, .) %>%
    ggplot()+
    geom_sf((aes(fill=Weighting)))+
    theme_void()+
    scale_fill_discrete()
}
