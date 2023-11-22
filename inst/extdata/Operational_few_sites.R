#### Operational Script for HyMETT functions on a few sites with observed and modeled data ####
#
#### Load libraries ####
library(HyMETT)
library(dplyr)

##### User inputs #####
OBS_dir <- paste0(system.file("extdata", package = "HyMETT"),"/")
MOD_dir <- paste0(system.file("extdata", package = "HyMETT"),"/")
out_dir <- paste0(normalizePath(tempdir(), winslash = "/"), "/")
site_no_list <- c("01013500","08202700",#Maine site, Texas zero-flow site
                  "01021470","01022260","01022500","01030500","01031500","01047000")

#### Initialize HyMETT run info DF ####
site_no_list <- as.data.frame(site_no_list); names(site_no_list) <- "site_no"
site_no_list$POR_start <- as.Date(NA)
site_no_list$POR_end <- as.Date(NA)
site_no_list$n_missing_days <- as.numeric(NA)
site_no_list$n_complete_WY <- as.numeric(NA)
site_no_list$complete_WY <- as.character(NA)
site_no_list$run_hymett <- as.logical(NA)
site_no_list$trend <- as.logical(NA)
site_no_list$fail <- as.logical(NA)

#### Empty DFs ####
ALL_obs_daily <- data.frame(matrix(nrow = 0, ncol = 13))
ALL_obs_annual <- data.frame(matrix(nrow = 0, ncol = 90))
ALL_obs_monthly <- data.frame(matrix(nrow = 0, ncol = 5))
ALL_obs_trend <- data.frame(matrix(nrow = 0, ncol = 10))

ALL_mod_daily <- data.frame(matrix(nrow = 0, ncol = 13))
ALL_mod_annual <- data.frame(matrix(nrow = 0, ncol = 90))
ALL_mod_monthly <- data.frame(matrix(nrow = 0, ncol = 5))
ALL_mod_trend <- data.frame(matrix(nrow = 0, ncol = 10))

ALL_comp_trend <- data.frame(matrix(nrow = 0, ncol = 5))
ALL_gof <- data.frame(matrix(nrow = 0, ncol = 5))
ALL_gof_report <- data.frame(matrix(nrow = 0, ncol = 15))

ALL_POR_annual_max <- data.frame(matrix(nrow = 0, ncol = 5))
ALL_POR_annual_min <- data.frame(matrix(nrow = 0, ncol = 5))
ALL_POR_daily_metrics <- data.frame(matrix(nrow = 0, ncol = 4))
ALL_POR_monthly_metrics <- data.frame(matrix(nrow = 0, ncol = 4))
site_no <- site_no_list$site_no[4]
#### Run for all sites ####
for(site_no in site_no_list$site_no){
  tryCatch({
    message(paste0("################################## Site Number: ",site_no," ################"))
    #### Load and pre-format data ####
    model <- read.csv(paste0(MOD_dir, site_no, "_MOD.csv"), header = T, stringsAsFactors = FALSE)
    model$Date <- as.Date(model$date, format = "%Y-%m-%d")
    
    observations <- read.csv(paste0(OBS_dir, site_no, "_OBS.csv"), header = T, stringsAsFactors = FALSE)
    observations$Date <- as.Date(observations$date, format = "%Y-%m-%d")
    
    # trim daily to common POR
    porStart <- max(min(model$Date), min(observations$Date))
    porEnd <- min(max(model$Date), max(observations$Date))
    
    model <- dplyr::filter(model, Date >= porStart, Date <= porEnd)
    observations <- dplyr::filter(observations, Date >= porStart, Date <= porEnd)
    site_no_list$POR_start[site_no_list$site_no == site_no] <- as.Date(porStart)
    site_no_list$POR_end[site_no_list$site_no == site_no] <- as.Date(porEnd)
    rm(porStart, porEnd)
    
    
    
    #### Pre-process data ####
    obs_WY <- HyMETT::preproc_main(Date = observations$Date, 
                                   value = observations$streamflow_cfs,
                                   date_format = "%Y-%m-%d",
                                   calc_WSCVD = FALSE)
    
    mod_WY <- HyMETT::preproc_main(Date = model$Date, 
                                   value = model$streamflow_cfs,
                                   date_format = "%Y-%m-%d",
                                   calc_WSCVD = FALSE)
    rm(observations, model)
    
    # count NA (ie. no value) days
    na_obs <- obs_WY$daily$Date[is.na(obs_WY$daily$value)]
    na_mod <- mod_WY$daily$Date[is.na(mod_WY$daily$value)]
    na_all <- unique(c(na_obs, na_mod))
    site_no_list$n_missing_days[site_no_list$site_no == site_no] <- length(na_all)
    rm(na_obs, na_mod, na_all)
    
    # trim to complete annual data
    compYears <- merge(obs_WY$audit, mod_WY$audit, by = "WY", all = TRUE, suffixes = c(".obs",".mod"))
    compYears <- dplyr::filter(compYears, complete.obs == TRUE, complete.mod == TRUE)
    
    obs_WY$annual <- obs_WY$annual[obs_WY$annual$WY %in% compYears$WY,]
    mod_WY$annual <- mod_WY$annual[mod_WY$annual$WY %in% compYears$WY,]
    site_no_list$n_complete_WY[site_no_list$site_no == site_no] <- nrow(compYears)
    if(nrow(compYears) == 0){
      site_no_list$complete_WY[site_no_list$site_no == site_no] <- NA
      site_no_list$run_hymett[site_no_list$site_no == site_no] <- FALSE
      site_no_list$fail[site_no_list$site_no == site_no] <- FALSE
      site_no_list$trend[site_no_list$site_no == site_no] <- FALSE
      next
    } else{
      site_no_list$complete_WY[site_no_list$site_no == site_no] <- paste(compYears$WY, collapse = ",")
      site_no_list$run_hymett[site_no_list$site_no == site_no] <- TRUE
    }
    
    #### Trends in annual statistics ####
    if(nrow(compYears) >= 8){
      obs_WY_trend <- lapply(obs_WY$annual, 
                             HyMETT::calc_annual_stat_trend, #FUN
                             year = obs_WY$annual$WY, data = NULL)       #args to FUN
      obs_WY_trend <- dplyr::bind_rows(obs_WY_trend, .id = "annual_stat")
      obs_WY_trend <- dplyr::filter(obs_WY_trend, annual_stat != "WY") #remove WY row
      
      mod_WY_trend <- lapply(mod_WY$annual, 
                             HyMETT::calc_annual_stat_trend, #FUN
                             year = mod_WY$annual$WY, data = NULL)       #args to FUN
      mod_WY_trend <- dplyr::bind_rows(mod_WY_trend, .id = "annual_stat")
      mod_WY_trend <- dplyr::filter(mod_WY_trend, annual_stat != "WY") #remove WY row
      
      # trend comparison
      comp_WY_trend <- tibble::tibble(annual_stat = mod_WY_trend$annual_stat,
                                      sen_slope_diff = mod_WY_trend$sen_slope - obs_WY_trend$sen_slope,
                                      trend_mag_diff = mod_WY_trend$trend_mag - obs_WY_trend$trend_mag,
                                      val_perc_change_diff = mod_WY_trend$val_perc_change - obs_WY_trend$val_perc_change)
      site_no_list$trend[site_no_list$site_no == site_no] <- TRUE
    } else{
      site_no_list$trend[site_no_list$site_no == site_no] <- FALSE
      obs_WY_trend <- NA
      mod_WY_trend <- NA
      comp_WY_trend <- NA
    }
    
    #### Goodness-of-Fit statistics ####
    gof_WY <- mapply(HyMETT::GOF_summary, 
                     mod_WY$annual, 
                     obs_WY$annual, 
                     MoreArgs = list(rmse_normalize = "range"),
                     SIMPLIFY = FALSE)
    gof_WY <- dplyr::bind_rows(gof_WY, .id = "annual_stat")
    gof_WY <- dplyr::filter(gof_WY, annual_stat != "WY") #remove WY row
    
    # format for report-style table
    gof_WY_report <- tidyr::pivot_wider(gof_WY, 
                                        id_cols = annual_stat, 
                                        names_from = metric, 
                                        values_from = c(value, p_value))
    gof_WY_report <- dplyr::select(gof_WY_report, !c(p_value_mean_absolute_error,
                                                     p_value_mean_error,
                                                     p_value_nash_sutcliffe_efficiency,
                                                     p_value_percent_bias,p_value_RMSE,
                                                     p_value_NRMSE,
                                                     p_value_volumetric_efficiency))
    
    #### Period-of-record statistics ####
    ## POR daily metrics
    POR_daily_obs <- POR_distribution_metrics(obs_WY$daily$value)
    POR_daily_mod <- POR_distribution_metrics(mod_WY$daily$value)
    # deseasonalized AR-1
    ar1_obs <- POR_calc_AR1(value = POR_deseasonalize(value = obs_WY$daily$value,
                                                      Date = obs_WY$daily$Date,
                                                      time_step = "daily"),
                            Date = obs_WY$daily$Date)
    ar1_mod <- POR_calc_AR1(value = POR_deseasonalize(value = mod_WY$daily$value,
                                                      Date = mod_WY$daily$Date,
                                                      time_step = "daily"),
                            Date = mod_WY$daily$Date)
    POR_daily_obs <- tibble::add_row(POR_daily_obs, metric = "lag1_autocorrelation", value = ar1_obs)
    POR_daily_mod <- tibble::add_row(POR_daily_mod, metric = "lag1_autocorrelation", value = ar1_mod)
    rm(ar1_obs, ar1_mod)
    # compute seasonal amplitude and phase of daily flows
    seas_obs <- POR_calc_amp_and_phase(value = obs_WY$daily$value,
                                       Date = obs_WY$daily$Date,
                                       time_step = "daily")
    seas_mod <- POR_calc_amp_and_phase(value = mod_WY$daily$value,
                                       Date = mod_WY$daily$Date,
                                       time_step = "daily")
    POR_daily_obs <- tibble::add_row(POR_daily_obs, metric = seas_obs$metric, value = seas_obs$value)
    POR_daily_mod <- tibble::add_row(POR_daily_mod, metric = seas_mod$metric, value = seas_mod$value)
    rm(seas_obs, seas_mod)
    # daily - label and combine
    POR_daily_obs <- tibble::add_column(POR_daily_obs, data = "observed", .before = 1)
    POR_daily_mod <- tibble::add_column(POR_daily_mod, data = "modeled", .before = 1)
    POR_daily_metrics <- rbind(POR_daily_obs, POR_daily_mod)
    rm(POR_daily_obs, POR_daily_mod)
    
    ## POR annual metrics
    POR_annual_low_obs <- POR_apply_annual_lowflow_stats(dplyr::select(obs_WY$annual, low_q1, low_q3, low_q7, low_q30))
    POR_annual_low_obs <- tibble::add_column(POR_annual_low_obs, data = "observed", .before = 1)
    POR_annual_low_mod <- POR_apply_annual_lowflow_stats(dplyr::select(mod_WY$annual, low_q1, low_q3, low_q7, low_q30))
    POR_annual_low_mod <- tibble::add_column(POR_annual_low_mod, data = "modeled", .before = 1)
    POR_annual_min <- rbind(POR_annual_low_obs, POR_annual_low_mod)
    rm(POR_annual_low_obs, POR_annual_low_mod)
    
    POR_annual_hi_obs <- POR_apply_annual_hiflow_stats(dplyr::select(obs_WY$annual, high_q1, high_q3, high_q7, high_q30))
    POR_annual_hi_obs <- tibble::add_column(POR_annual_hi_obs, data = "observed", .before = 1)
    POR_annual_hi_mod <- POR_apply_annual_hiflow_stats(dplyr::select(mod_WY$annual, high_q1, high_q3, high_q7, high_q30))
    POR_annual_hi_mod <- tibble::add_column(POR_annual_hi_mod, data = "modeled", .before = 1)
    POR_annual_max <- rbind(POR_annual_hi_obs, POR_annual_hi_mod)
    rm(POR_annual_hi_obs, POR_annual_hi_mod)
    
    ## POR monthly metrics
    # monthly mean time-series
    obs_WY$monthly <- obs_WY$daily %>%
      group_by(year, month) %>% dplyr::summarise(value = mean(value))
    mod_WY$monthly <- mod_WY$daily %>%
      group_by(year, month) %>% dplyr::summarise(value = mean(value))
    # monthly metrics
    POR_monthly_obs <- POR_distribution_metrics(obs_WY$monthly$value)
    POR_monthly_mod <- POR_distribution_metrics(mod_WY$monthly$value)
    # AR-1
    obs_WY$monthly$date_mid <- as.Date(paste(obs_WY$monthly$year, obs_WY$monthly$month, "15", sep = "-"))
    mod_WY$monthly$date_mid <- as.Date(paste(mod_WY$monthly$year, mod_WY$monthly$month, "15", sep = "-"))
    ar1_obs <- POR_calc_AR1(value = POR_deseasonalize(value = obs_WY$monthly$value,
                                                      Date = obs_WY$monthly$date_mid,
                                                      time_step = "monthly"),
                            Date = obs_WY$monthly$date_mid,
                            time_step = "monthly")
    ar1_mod <- POR_calc_AR1(value = POR_deseasonalize(value = mod_WY$monthly$value,
                                                      Date = mod_WY$monthly$date_mid,
                                                      time_step = "monthly"),
                            Date = mod_WY$monthly$date_mid,
                            time_step = "monthly")
    POR_monthly_obs <- tibble::add_row(POR_monthly_obs, metric = "lag1_autocorrelation", value = ar1_obs)
    POR_monthly_mod <- tibble::add_row(POR_monthly_mod, metric = "lag1_autocorrelation", value = ar1_mod)
    rm(ar1_obs, ar1_mod)
    # seasonality
    seas_obs <- POR_calc_amp_and_phase(value = obs_WY$monthly$value,
                                       Date = obs_WY$monthly$date_mid,
                                       time_step="monthly")
    seas_mod <- POR_calc_amp_and_phase(value = mod_WY$monthly$value,
                                       Date = mod_WY$monthly$date_mid,
                                       time_step="monthly")
    POR_monthly_obs <- tibble::add_row(POR_monthly_obs, metric = seas_obs$metric, value = seas_obs$value)
    POR_monthly_mod <- tibble::add_row(POR_monthly_mod, metric = seas_mod$metric, value = seas_mod$value)
    rm(seas_obs, seas_mod)
    
    # monthly - label and combine
    POR_monthly_obs <- tibble::add_column(POR_monthly_obs, data = "observed", .before = 1)
    POR_monthly_mod <- tibble::add_column(POR_monthly_mod, data = "modeled", .before = 1)
    POR_monthly_metrics <- rbind(POR_monthly_obs, POR_monthly_mod)
    rm(POR_monthly_obs, POR_monthly_mod)
    
    #### Logistic regression for zero-flow annual statistics ####
    if(nrow(compYears) >= 8){
      obs_zero <- attr(obs_WY$annual, "zero_flow_years")
      # select statistics over the zero threshold
      obs_zero <- dplyr::select(obs_WY$annual, "WY", 
                                obs_zero$annual_stat[obs_zero$over_threshold == TRUE & 
                                                       !is.na(obs_zero$over_threshold)])
      if(ncol(obs_zero) > 1){
        obs_WY_lr <- lapply(obs_zero, 
                            HyMETT::calc_logistic_regression, 
                            year = obs_zero$WY, data = NULL)
        obs_WY_lr <- dplyr::bind_rows(obs_WY_lr, .id = "annual_stat")
        obs_WY_lr <- dplyr::filter(obs_WY_lr, annual_stat != "WY") #remove WY row
      } else{obs_WY_lr <- NA}
      
      mod_zero <- attr(mod_WY$annual, "zero_flow_years")
      # select statistics over the zero threshold
      mod_zero <- dplyr::select(mod_WY$annual, "WY", 
                                mod_zero$annual_stat[mod_zero$over_threshold == TRUE & 
                                                       !is.na(mod_zero$over_threshold)])
      if(ncol(mod_zero) > 1){
        mod_WY_lr <- lapply(mod_zero, 
                            HyMETT::calc_logistic_regression, 
                            year = mod_zero$WY, data = NULL)
        mod_WY_lr <- dplyr::bind_rows(mod_WY_lr, .id = "annual_stat")
        mod_WY_lr <- dplyr::filter(mod_WY_lr, annual_stat != "WY") #remove WY row
      } else{mod_WY_lr <- NA}
      rm(obs_zero, mod_zero)
    } else{
      obs_WY_lr <- NA
      mod_WY_lr <- NA
    }
    rm(compYears)
    
    
    #### Add site number and output tables ####
    # preproc data
    obs_WY$daily <- tibble::add_column(obs_WY$daily, site_no = site_no, .before = 1)
    obs_WY$annual <- tibble::add_column(obs_WY$annual, site_no = site_no, .before = 1)
    obs_WY$monthly <- tibble::add_column(obs_WY$monthly, site_no = site_no, .before = 1)
    
    mod_WY$daily <- tibble::add_column(mod_WY$daily, site_no = site_no, .before = 1)
    mod_WY$annual <- tibble::add_column(mod_WY$annual, site_no = site_no, .before = 1)
    mod_WY$monthly <- tibble::add_column(mod_WY$monthly, site_no = site_no, .before = 1)
    
    # trend
    if(is.data.frame(obs_WY_trend)){obs_WY_trend <- tibble::add_column(obs_WY_trend, site_no = site_no, .before = 1)}
    if(is.data.frame(mod_WY_trend)){mod_WY_trend <- tibble::add_column(mod_WY_trend, site_no = site_no, .before = 1)}
    if(is.data.frame(comp_WY_trend)){comp_WY_trend <- tibble::add_column(comp_WY_trend, site_no = site_no, .before = 1)}
    
    # GOF
    gof_WY <- tibble::add_column(gof_WY, site_no = site_no, .before = 1)
    gof_WY_report <- tibble::add_column(gof_WY_report, site_no = site_no, .before = 1)
    
    # POR
    POR_annual_max <- tibble::add_column(POR_annual_max, site_no = site_no, .before = 1)
    POR_annual_min <- tibble::add_column(POR_annual_min, site_no = site_no, .before = 1)
    POR_daily_metrics <- tibble::add_column(POR_daily_metrics, site_no = site_no, .before = 1)
    POR_monthly_metrics <- tibble::add_column(POR_monthly_metrics, site_no = site_no, .before = 1)
    
    # Logistic Regression
    if(!is.na(obs_WY_lr)){obs_WY_lr <- tibble::add_column(obs_WY_lr, site_no = site_no, .before = 1)}
    if(!is.na(mod_WY_lr)){mod_WY_lr <- tibble::add_column(mod_WY_lr, site_no = site_no, .before = 1)}
    
    # output
    write.table(obs_WY$daily, file = paste0(out_dir,site_no,"_obs_daily.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(obs_WY$annual, file = paste0(out_dir,site_no,"_obs_annual.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(obs_WY$monthly, file = paste0(out_dir,site_no,"_obs_monthly.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(mod_WY$daily, file = paste0(out_dir,site_no,"_mod_daily.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(mod_WY$annual, file = paste0(out_dir,site_no,"_mod_annual.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(mod_WY$monthly, file = paste0(out_dir,site_no,"_mod_monthly.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    if(is.data.frame(obs_WY_trend)){write.table(obs_WY_trend, file = paste0(out_dir,site_no,"_obs_trend.csv"), sep = ",", quote = TRUE, row.names = FALSE)}
    if(is.data.frame(mod_WY_trend)){write.table(mod_WY_trend, file = paste0(out_dir,site_no,"_mod_trend.csv"), sep = ",", quote = TRUE, row.names = FALSE)}
    if(is.data.frame(comp_WY_trend)){write.table(comp_WY_trend, file = paste0(out_dir,site_no,"_com_trend.csv"), sep = ",", quote = TRUE, row.names = FALSE)}
    write.table(gof_WY, file = paste0(out_dir,site_no,"_GOF.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(gof_WY_report, file = paste0(out_dir,site_no,"_GOF_report.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(POR_annual_max, file = paste0(out_dir,site_no,"_POR_annual_max.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(POR_annual_min, file = paste0(out_dir,site_no,"_POR_annual_min.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(POR_daily_metrics, file = paste0(out_dir,site_no,"_POR_daily_metrics.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    write.table(POR_monthly_metrics, file = paste0(out_dir,site_no,"_POR_monthly_metrics.csv"), sep = ",", quote = TRUE, row.names = FALSE)
    
    # append to R dataframes
    ALL_obs_daily <- rbind(ALL_obs_daily, obs_WY$daily)
    ALL_obs_annual <- rbind(ALL_obs_annual, obs_WY$annual)
    ALL_obs_monthly <- rbind(ALL_obs_monthly, obs_WY$monthly)
    
    ALL_mod_daily <- rbind(ALL_mod_daily, mod_WY$daily)
    ALL_mod_annual <- rbind(ALL_mod_annual, mod_WY$annual)
    ALL_mod_monthly <- rbind(ALL_mod_monthly, mod_WY$monthly)
    
    trend <- site_no_list$trend[site_no_list$site_no == site_no]
    if(trend == TRUE){
      ALL_obs_trend <- rbind(ALL_obs_trend, obs_WY_trend)
      ALL_mod_trend <- rbind(ALL_mod_trend, mod_WY_trend)
      ALL_comp_trend <- rbind(ALL_comp_trend, comp_WY_trend)
    }
    rm(trend)
    
    ALL_gof <- rbind(ALL_gof, gof_WY)
    ALL_gof_report <- rbind(ALL_gof_report, gof_WY_report)
    
    ALL_POR_annual_max <- rbind(ALL_POR_annual_max, POR_annual_max)
    ALL_POR_annual_min <- rbind(ALL_POR_annual_min, POR_annual_min)
    ALL_POR_daily_metrics <- rbind(ALL_POR_daily_metrics, POR_daily_metrics)
    ALL_POR_monthly_metrics <- rbind(ALL_POR_monthly_metrics, POR_monthly_metrics)
    
    # cleanup
    site_no_list$fail[site_no_list$site_no == site_no] <- FALSE
    rm(list = ls()[!(ls() %in% c("ALL_comp_trend", "ALL_gof", "ALL_gof_report", 
                                 "ALL_mod_annual", "ALL_mod_daily", "ALL_mod_monthly", "ALL_mod_trend",
                                 "ALL_obs_annual", "ALL_obs_daily","ALL_obs_monthly", "ALL_obs_trend",
                                 "ALL_POR_annual_max",
                                 "ALL_POR_annual_min",
                                 "ALL_POR_daily_metrics",
                                 "ALL_POR_monthly_metrics",
                                 "MOD_dir", "OBS_dir", "out_dir", "site_no_list"))])
    
  },
  error = function(e){
    message(print(e))
  })#end of trycatch
}
site_no_list$fail[is.na(site_no_list$fail)] <- TRUE
write.table(site_no_list, file = paste0(out_dir,"HyMETT_run.txt"), sep = "\t", quote = T, row.names = F, col.names = T)

