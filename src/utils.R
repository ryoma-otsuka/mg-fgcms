#### Libraries ####
library(dplyr)
library(rstan)
library(rethinking)
library(loo)
library(tidybayes)
library(ggplot2)
library(svglite)
library(extrafont)
extrafont::loadfonts(device = "win")


#### Setup ####
setup = function(use_stan=TRUE, is_rmd=FALSE)
{
  # visualization setup
  ggplot() +
    theme_set(
      theme_bw() +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.text = element_text(size = 12)) + 
        theme(axis.title.y = element_text(
          size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0))
        ) + 
        theme(legend.title = element_text(size = 12)) + 
        theme(legend.text = element_text(size = 10)) +
        theme(axis.ticks.x = element_line(linewidth = 0.7)) +
        theme(axis.ticks.y = element_line(linewidth = 0.7)) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10))) +
        theme(panel.border = element_rect(size = 1.2, color = "black")) +
        theme(strip.background = element_rect(size = 1.2, color = "black")) +
        theme(strip.text = element_text(size = 12)) +
        # theme(panel.grid.major.x = element_blank()) +
        # theme(panel.grid.minor.x = element_blank()) +
        # theme(panel.grid.major.y = element_line(colour = "grey90")) +
        # theme(panel.grid.minor.y = element_line(colour = "grey90")) +
        theme(plot.margin = unit(c(5, 5, 5, 5), "mm")) + 
        theme(text = element_text(family = "Helvetica")) +
        theme(axis.text = element_text(color = "black"))
    )
  
  if (use_stan)
  {
    # for speeding up MCMC sampling
    rstan_options(auto_write = TRUE)
    # options(mc.cores = parallel::detectCores())
  }
  
  if (is_rmd)
  {
    library(knitr)
    # Grobal options
    opts_knit$set(root.dir = "fgcms/")
    knitr::opts_chunk$set(
      echo = TRUE,
      cache = TRUE,
      prompt = FALSE,
      tidy = TRUE,
      comment = NA,
      message = TRUE,
      warning = TRUE
    )
  }
}


#### Slope plot ####
vis_slope_plot = function(df, x_labels, y_lim, log_scale=FALSE) {
  
  color_mapping = c("Even"="grey", "Negative"="#1E88E5", "Positive"="#D81B60")
  
  if (log_scale == TRUE)
  {
    df$Values = log(df$Values)
  }
  p = ggplot(data = df, aes(x=Condition, y=Values)) +
    
    # violin plot in grey, no outline
    geom_violin(alpha=0.5, fill="grey80", color=NA, show.legend = FALSE) +
    # data
    geom_point(col="grey25", size=3, shape=1) +
    # slope
    geom_line(aes(col=Difference, group=ID), linewidth=0.50) +
    # color
    scale_color_manual(values = color_mapping) +
    scale_fill_manual(values = color_mapping) +
    # show box plots
    geom_boxplot(
      alpha=0.2, 
      fill=NA, 
      outlier.shape = NA, 
      notch = FALSE,
      width = 0.2, 
      size = 0.7,   # box line
      fatten = 1.5, # median line 
      # position = position_dodge(width = 2.0)
      position = position_nudge(x = c(-0.2, 0.2))
    ) 
  
  if (log_scale == TRUE)
  {
    p = p + scale_y_continuous(breaks = seq(from=0, to=12, by=1)) +
      labs(y="log-fGCMs (ng/g wet)")
  } else
  {
    p = p + scale_y_continuous(breaks = seq(from=0, to=1400, by=200)) +
      labs(y="fGCMs (ng/g wet)")
  }
  
  p = p +
    # settings
    scale_x_discrete(labels = x_labels) +
    coord_cartesian(ylim = y_lim) +
    theme(legend.position = "none") +
    theme(axis.title.x = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(text = element_text(family = "Helvetica")) +
    NULL
  
  return(p)
}


prep_df_slope = function(df, exp_type='volume') {
  
  # str(df)
  # summary(df)
  length(df[,1])
  
  if (exp_type == 'volume')
  {
    Diff = df$X05g5ml  - df$X1g8ml
  } else if (exp_type == 'filter')
  {
    Diff = df$Filter  - df$No_filter
  } else if (exp_type == 'nest')
  {
    Diff = df$Nest - df$Trail
  }
  
  head(Diff)
  
  diff = c(rep(NA, length(Diff)))
  
  for (i in 1:length(Diff)) {
    if (Diff[i] > 0) {
      diff[i] = "Positive"
    }
    else if (Diff[i] == 0) {
      diff[i] = "Even"
    }
    else diff[i] = "Negative"
  }
  
  # head(diff)
  
  # Long
  if (exp_type == 'volume') 
  {
    df1 = df[, -c(1:2)]
    df2 = df1 %>% 
      rename("0.50g/5ml" = "X05g5ml", "1.00g/8ml" = "X1g8ml") %>% 
      tidyr::gather(key = Condition, value = Values) %>% 
      dplyr::mutate(ID = rep(c(1:length(df[,1])), 2)) %>% 
      dplyr::mutate(Difference = rep(diff, 2))
  } else if (exp_type == 'filter')
  {
    df2 = df %>% 
      tidyr::gather(key = Condition, value = Values) %>% 
      dplyr::mutate(ID = rep(c(1:length(df[,1])), 2)) %>% 
      dplyr::mutate(Difference = rep(diff, 2))
  } else if (exp_type == 'nest')
  {
    df1 = df[, -c(1:4)]
    df2 = df1 %>% 
      tidyr::gather(key = Condition, value = Values) %>% 
      dplyr::mutate(ID = rep(c(1:length(df[,1])), 2)) %>% 
      dplyr::mutate(Difference = rep(diff, 2))
  }
  
  # print(df2)
  return (df2)
}



#### Data pre-processing ####

convert_gorilla_id_char_to_int = function(df, group_list)
{
  # set GID (Gorilla ID)
  if (length(group_list) == 1 && group_list[[1]] == "R")
  {
    df = df %>% dplyr::mutate(
      GID = dplyr::case_when(
        # Rushegura
        Gorilla_ID == "Barekye" ~ 1,
        Gorilla_ID == "Buzinza" ~ 2,
        Gorilla_ID == "Kabukojo" ~ 3,
        Gorilla_ID == "Kabunga" ~ 4,
        Gorilla_ID == "Kankwehe" ~ 5,
        Gorilla_ID == "Kanyindo" ~ 6,
        Gorilla_ID == "Kanywani" ~ 7,
        Gorilla_ID == "Karembezi" ~ 8,
        Gorilla_ID == "Kibande" ~ 9,
        Gorilla_ID == "Masanyu" ~ 10,
        Gorilla_ID == "Mitunu" ~ 11,
        Gorilla_ID == "Munyana" ~ 12,
        Gorilla_ID == "Nyampazi" ~ 13,
        Gorilla_ID == "Rushokye" ~ 14,
        Gorilla_ID == "Ruterana" ~ 15,
        TRUE ~ NA_real_
      )
    )
  } else if (length(group_list) == 2 && all(group_list == c("R", "M")))
  {
    df = df %>% dplyr::mutate(
      GID = dplyr::case_when(
        # Rushegura
        Gorilla_ID == "Barekye" ~ 1,
        Gorilla_ID == "Buzinza" ~ 2,
        Gorilla_ID == "Kabukojo" ~ 3,
        Gorilla_ID == "Kabunga" ~ 4,
        Gorilla_ID == "Kankwehe" ~ 5,
        Gorilla_ID == "Kanyindo" ~ 6,
        Gorilla_ID == "Kanywani" ~ 7,
        Gorilla_ID == "Karembezi" ~ 8,
        Gorilla_ID == "Kibande" ~ 9,
        Gorilla_ID == "Masanyu" ~ 10,
        Gorilla_ID == "Mitunu" ~ 11,
        Gorilla_ID == "Munyana" ~ 12,
        Gorilla_ID == "Nyampazi" ~ 13,
        Gorilla_ID == "Rushokye" ~ 14,
        Gorilla_ID == "Ruterana" ~ 15,
        # Mubare
        Gorilla_ID == "Karungi" ~ 16,
        Gorilla_ID == "Kikombe" ~ 17,
        Gorilla_ID == "Kisho" ~ 18,
        Gorilla_ID == "Maraya" ~ 19,
        Gorilla_ID == "Twesiime" ~ 20,
        TRUE ~ NA_real_
      )
    )
  }
  return (df)
}



load_and_preprocess_data = function(path, group_list=c("R")) 
{
  # load data
  df = read.csv(path)
  df = df %>%
    dplyr::select(-c(Sample_ID)) %>%  # remove a column of Date
    dplyr::mutate(FGCMs = FGCMs_ng_g_wet) %>%
    dplyr::mutate(log_FGCMs = log(FGCMs))  # add a new column (log transformed FGCMs concentration)
  # print(str(df))
  print(sprintf("N = %s (All data)", length(df[,1])))
  
  # remove outlier
  df = subset(df, FGCMs < 8000)
  print(sprintf("N = %s (After removing outlier, >= 8000)", length(df[,1])))
  
  # remove some rows that you don't need
  df = dplyr::filter(
    df,
    Year %in% c("2018"),                                      # remove 2017 samples
    Sex %in% c("F", "M"),                                     # remove NA in Sex
    Age_Class %in% c("SB", "BB", "AdF", "SAd", "Juv", "Inf"), # remove NA in Class
    Group %in% group_list,                                    # remove unnecessary group
    Place %in% c("Trail"),                                    # remove Nest samples
  )
  
  if (length(group_list) == 1 && group_list[[1]] == "R")
  {
    df = dplyr::filter(
      df,
      !is.na(Tourist_number_2d),                               # remove NA
      !is.na(Tourist_visit_2d),                                # remove NA
      !is.na(Mean_temp),                                       # remove NA
    )
  }
  print(sprintf("N = %s (After removing NA, 2017, nest, unused group samples)", length(df[,1])))
  
  # set GID
  df = convert_gorilla_id_char_to_int(df, group_list)
  
  # set factors
  df = df %>% dplyr::mutate(Sex = ifelse(Sex == "F", "Female", "Male"))
  df$Sex = factor(df$Sex, levels=c("Female", "Male"))
  df$Age_Class = factor(df$Age_Class, levels=c("SB", "BB", "AdF", "SAd", "Inf"))
  df = df %>% dplyr::mutate(Age_Category = ifelse(Age_Class == "Inf", "Infant", "Adult")) 
  df$Age_Category = factor(df$Age_Category, levels=c("Adult", "Infant"))
  df$Tourist_visit_2d = as.factor(df$Tourist_visit_2d)
  
  # standardization (centering + scaling)
  df = df %>%
    dplyr::mutate(
      s_Tourist_number_2d = (Tourist_number_2d - mean(Tourist_number_2d)) / sd(Tourist_number_2d),
      s_Mean_temp = (Mean_temp - mean(Mean_temp)) / sd(Mean_temp)
    )
  print(summary(df))
  return (df)
  
}



#### MCMC Convergence Diagnosis ####
check_rhat = function(fit, threshold_list=c(1.010, 1.005))
{
  for (i in 1:length(threshold_list))
  {
    threshold = threshold_list[[i]]
    is_ok = all(summary(fit)$summary[, 'Rhat'] < threshold, na.rm = T)
    print(sprintf("all rhat value < threshold: %.3f? %s", threshold, is_ok))
  }
  return (is_ok)
}

check_divergent_transitions = function(fit)
{
  sampler_params = get_sampler_params(fit, inc_warmup=FALSE)
  divergent_count = sapply(sampler_params, function(x) sum(x[, "divergent__"]))
  total_divergent = sum(divergent_count)
  print(sprintf("N of divergent transition: %s", total_divergent))
  
  
  
  return (total_divergent)
}

vis_traceplot = function(
    fit, 
    parameters, 
    ncol=5, 
    inc_warmup=FALSE,
    separate_chains=TRUE
)
{
  
  # color set
  # colors = c("#d62728", "#2ca02c", "#ff7f0e", "#1f77b4")
  colors = c('#F0E442', '#E69F00', '#56B4E9', '#009E73')
  # colors = c('#CC79A7', '#E69F00', '#56B4E9', '#009E73')
  # colors = c('#fde725', '#35b779',  '#31688e', '#440154')
  # colors = c('#ffd700', '#fa8775',  '#cd34b5', '#0000ff')
  
  # base
  p_set = theme_bw(base_size=12) +
    theme(axis.text=element_text(color="black"))
  
  # trace plot
  p_traceplot = rstan::traceplot(
    fit, pars=parameters,
    ncol=ncol, inc_warmup=inc_warmup
  ) +
    scale_color_manual(values=colors) +
    p_set + 
    NULL
  print(p_traceplot)
  
  return (p_traceplot)
}


vis_histogram = function(
    fit, 
    parameters, 
    ncol=5, 
    inc_warmup=FALSE,
    separate_chains=TRUE
)
{
  
  # color set
  # colors = c("#d62728", "#2ca02c", "#ff7f0e", "#1f77b4")
  colors = c('#F0E442', '#E69F00', '#56B4E9', '#009E73')
  
  # base
  p_set = theme_bw(base_size=12) +
    theme(axis.text=element_text(color="black"))
  
  # histogram
  p_histogram = rstan::stan_hist(
    fit, pars=parameters, ncol=ncol, 
    fill="#009E73", col="transparent", alpha=0.7, bins=30
  ) + 
    geom_vline(xintercept = c(0), linetype = "dashed") +
    labs(y="Count") + 
    p_set +
    NULL
  print(p_histogram)
  
  return (p_histogram)
}


vis_density = function(
    fit, 
    parameters, 
    ncol=5, 
    inc_warmup=FALSE,
    separate_chains=TRUE
)
{
  
  # color set
  # colors = c("#d62728", "#2ca02c", "#ff7f0e", "#1f77b4")
  colors = c('#F0E442', '#E69F00', '#56B4E9', '#009E73')
  
  # base
  p_set = theme_bw(base_size=12) +
    theme(axis.text=element_text(color="black"))
  
  # kernel density plot
  p_density = rstan::stan_dens(
    fit, pars=parameters, ncol=ncol, 
    separate_chains=separate_chains, alpha=0.5
  ) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    # scale_color_manual(values=colors) + 
    scale_fill_manual(values=colors) + 
    labs(y="Density") + 
    p_set + 
    NULL
  print(p_density)
  
  return (p_density)
}

#### Posterior Predictive Checks ####
vis_posterior_predictive_check = function(fit, Y, title, I=100, color="#56B4E9")
{
  
  # base plot
  p3 = ggplot() +
    geom_density(mapping=aes(x=Y), size=0.75, trim=TRUE) +
    scale_x_continuous(breaks=seq(from=0, to=20, by=1)) +
    scale_y_continuous(breaks=seq(from=0, to=2, by=0.2)) +
    coord_cartesian(xlim=c(3, 8), ylim=c(0.0, 1.2)) +
    labs(title = title, x = "log-fGCMs (ng/g wet)", y = "Density") +
    # theme(panel.grid.minor.x = element_line(linewidth = 0.5), 
    #       panel.grid.major.x = element_line(linewidth = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
    NULL
  # print(p3)
  
  # just plot the first 100 simulated data
  ms = rstan::extract(fit)
  Y_sim = rep(NA, ncol(ms$Y_sim))
  df_sim = data.frame(Y_sim = Y_sim)
  
  # Randomly extract 100 columns and plot their posterior prediction density
  set.seed(1234)
  columns_to_plot = sample(1:ncol(ms$Y_sim), 100) # a vector for 
  for (i in columns_to_plot) {
    df_sim$Y_sim = ms$Y_sim[i, ]
    p3 = p3 + 
      geom_density(data=df_sim, mapping=aes(x=Y_sim), color=color, size=0.2, trim=TRUE)
  }
  p3 = p3 + geom_density(mapping = aes(x=Y), size=1.0, trim=TRUE)
  # print(p3)
  return (p3)
}


#### Posterior distributions ####
vis_slope_posteriors = function(fit, title, xlim_factor=1.0, color="#56B4E9")
{
  
  # n of beta dims
  beta_samples = rstan::extract(fit)$b
  beta_dim = ncol(beta_samples)
  if (is.na(beta_dim))
  {
    beta_dim = 1
  }
  print(sprintf("beta_dim: %s", beta_dim))
  
  max_val = max(beta_samples)
  min_val = min(beta_samples)
  extreme_val = ifelse(abs(max_val) > abs(min_val), max_val, min_val)

  axis_limit = abs(extreme_val) * xlim_factor
  xlim_lower = -axis_limit
  xlim_upper = axis_limit 
  
  # plot posteriors
  
  if (beta_dim > 1)
  {
    p = fit %>%
      spread_draws(b[K]) %>%
      ggplot(aes(y = factor(K, rev(1:beta_dim)), x = b)) +
      # geom_vline(xintercept = c(0), linetype = "dashed", alpha=0.1) +
      # posterior distribution + 89%, 97% CI
      stat_halfeye(fill=color, .width = c(0.89, 0.97), slab_alpha=0.7) + 
      scale_y_discrete(
        labels = sapply(rev(seq_len(beta_dim)), function(i) bquote(beta[.(i)]))
      )
  } else 
  {
    p = fit %>%
      spread_draws(b1) %>%
      ggplot(aes(y = factor(1), x = b1)) +
      # geom_vline(xintercept = c(0), linetype = "dashed", alpha=0.1) +
      # posterior distribution + 89%, 97% CI
      stat_halfeye(fill=color, .width = c(0.89, 0.97), slab_alpha=0.7) + 
      scale_y_discrete(
        labels = bquote(beta[1])  # beta_1 を表示する
      )
  }
  
  p = p +
    labs(
      title = sprintf("%s", title),
      x = "Value",
      y = "Parameter"
    ) + 
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    # theme(panel.grid.major.x = element_line(colour = "grey80")) +
    # theme(panel.grid.minor.x = element_line(colour = "grey90")) +
    theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))
  
  # set xlim
  p = p + xlim(xlim_lower, xlim_upper)
  
  # Add text | Posterior median + 97 % CI
  if (beta_dim > 1)
  {
    posterior_summary = fit %>%
      spread_draws(b[K]) %>%
      group_by(K) %>%
      summarise(
        median = median(b),
        lower = quantile(b, 0.015),
        upper = quantile(b, 0.985)
      )
  } else 
  {
    posterior_summary = fit %>%
      spread_draws(b1) %>%
      summarise(
        median = median(b1),
        lower = quantile(b1, 0.015),
        upper = quantile(b1, 0.985)
      )
  }
  
  y_offset = 0.20
  x_offset = 0.00
  
  if (beta_dim > 1)
  {
    p = p + 
      geom_text(
        data = posterior_summary,
        aes(
          factor(K, rev(1:beta_dim)) %>% as.numeric() + y_offset,
          x = median + x_offset,
          label = sprintf("%.3f [%.3f, %.3f]", median, lower, upper)
        ),
        color = "#000000",
        hjust = 0.5,  # centering
        lineheight = 0.8  # line space
      )
  } else 
  {
    p = p + 
      geom_text(
        data = posterior_summary,
        aes(
          factor(c(1)) %>% as.numeric() + y_offset,
          x = median + x_offset,
          label = sprintf("%.3f [%.3f, %.3f]", median, lower, upper)
        ),
        color = "#000000",
        hjust = 0.5,  # centering
        lineheight = 0.8  # line space
      ) 
  }
  
  
  # print(p)
  return (p)
}


plot_parallelism_test = function(df, y_target="Absorbance", ncol=5)
{
  color_list = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#333333')
  # color_list = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2')
  
  #### Parallelism check ####
  
  if (y_target == "Absorbance")
  {
    p = ggplot(mapping=aes(x = Relative_Dilution_factor, y = Mean, col = Type3), data = df) +
      labs(x = "Relative Dilution Factor", y = "Absorbance", col = NULL)
  } else if (y_target == "Conc")
  {
    p = ggplot(mapping=aes(x = Relative_Dilution_factor, y = Conc, col = Type3), data = df) +
      labs(x = "Relative Dilution Factor", y = "Conc.", col = NULL)
  } else
  {
    print("Error")
    return (0)
  }
  
  p = p +
    geom_smooth(method = lm, se = FALSE, linewidth = 0.9, linetype="solid") + # linear
    # geom_smooth(method = lm,  formula = y ~ poly(x, 3), se = FALSE, linewidth = 0.9, linetype="solid") + # cubic
    geom_line(linewidth = 0.9, linetype="dashed") +
    geom_point(size=3.0) +
    scale_x_log10(limits = c(0.1, 100),
                  breaks = c(0.1, 1, 10, 100),
                  labels = c("0.1", "1", "10", "100")
                  # labels=scales::number_format(accuracy = 0.01)
    ) +
    scale_y_continuous(limits = c(0.0, 2.0)) +
    coord_cartesian(xlim = c(0.1, 200), ylim = c(0.0, 2.0)) +
    scale_color_manual(values = color_list) +
    theme(axis.title.x = element_text(margin = margin(t = 10))) +
    theme(axis.title.y = element_text(margin = margin(r = 10))) +
    theme(panel.grid.major.x = element_line(colour = "grey90")) +
    theme(panel.grid.minor.x = element_line(colour = "grey90")) +
    theme(panel.grid.major.y = element_line(colour = "grey90")) +
    theme(panel.grid.minor.y = element_line(colour = "grey90")) +
    theme(text = element_text(family = "Helvetica")) +
    facet_wrap(~ Type3, ncol=ncol) +
    theme(legend.position = "none") +
    NULL
  print(p)
  return (p)
}
