clock_start <- Sys.time()
#### Load libraries ####
library(HyMETT)
library(dplyr)

##### User inputs #####
out_dir <- paste0(normalizePath(tempdir(), winslash = "/"), "/")
run_info <- read.table(file = paste0(out_dir,"HyMETT_run.txt"), sep = "\t",
                       colClasses = c("character", rep("Date",2), rep("numeric",2) , "character", 
                                      rep("logical",3)), header = TRUE)
not_run <- run_info[run_info$run_hymett == FALSE,]
run_info <- run_info[run_info$run_hymett == TRUE,]

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

# output
for(site_no in run_info$site_no){
  message(paste0("################################## Site Number: ",site_no," ################"))
  
  x <- read.csv(file = paste0(out_dir,site_no,"_obs_daily.csv"), colClasses = c("character", "Date", rep("numeric",11)))
  x2 <- read.csv(file = paste0(out_dir,site_no,"_obs_annual.csv"), colClasses = c("character", rep("numeric",24)))
  x3 <- read.csv(file = paste0(out_dir,site_no,"_obs_monthly.csv"), colClasses = c("character", rep("numeric",3), "Date"))
  ALL_obs_daily <- rbind(ALL_obs_daily, x)
  ALL_obs_annual <- rbind(ALL_obs_annual, x2)
  ALL_obs_monthly <- rbind(ALL_obs_monthly, x3)
  rm(x,x2,x3)
  
  y <- read.csv(file = paste0(out_dir,site_no,"_mod_daily.csv"), colClasses = c("character", "Date", rep("numeric",11)))
  y2 <- read.csv(file = paste0(out_dir,site_no,"_mod_annual.csv"), colClasses = c("character", rep("numeric",24)))
  y3 <- read.csv(file = paste0(out_dir,site_no,"_mod_monthly.csv"), colClasses = c("character", rep("numeric",3), "Date"))
  ALL_mod_daily <- rbind(ALL_mod_daily, y)
  ALL_mod_annual <- rbind(ALL_mod_annual, y2)
  ALL_mod_monthly <- rbind(ALL_mod_monthly, y3)
  rm(y, y2, y3)
  
  trend <- run_info$trend[run_info$site_no == site_no]
  if(trend == TRUE){
    x4 <- read.csv(file = paste0(out_dir,site_no,"_obs_trend.csv"), colClasses = c(rep("character",2), rep("numeric",8)))
    y4 <- read.csv(file = paste0(out_dir,site_no,"_mod_trend.csv"), colClasses = c(rep("character",2), rep("numeric",8)))
    z <- read.csv(file = paste0(out_dir,site_no,"_com_trend.csv"), colClasses = c(rep("character",2), rep("numeric",3)))
    
    ALL_obs_trend <- rbind(ALL_obs_trend, x4)
    ALL_mod_trend <- rbind(ALL_mod_trend, y4)
    ALL_comp_trend <- rbind(ALL_comp_trend, z)
    rm(x4,y4,z)
  }
  rm(trend)
  
  z2 <- read.csv(file = paste0(out_dir,site_no,"_GOF.csv"), colClasses = c(rep("character",3), rep("numeric",2)))
  z3 <- read.csv(file = paste0(out_dir,site_no,"_GOF_report.csv"), colClasses = c(rep("character",2), rep("numeric",13)))
  ALL_gof <- rbind(ALL_gof, z2)
  ALL_gof_report <- rbind(ALL_gof_report, z3)
  rm(z2,z3)
  
  z4 <- read.csv(file = paste0(out_dir,site_no,"_POR_annual_max.csv"), colClasses = c(rep("character",3), rep("numeric",2)))
  z5 <- read.csv(file = paste0(out_dir,site_no,"_POR_annual_min.csv"), colClasses = c(rep("character",4), "numeric"))
  z6 <- read.csv(file = paste0(out_dir,site_no,"_POR_daily_metrics.csv"), colClasses = c(rep("character",3), "numeric"))
  z7 <- read.csv(file = paste0(out_dir,site_no,"_POR_monthly_metrics.csv"), colClasses = c(rep("character",3), "numeric"))
  ALL_POR_annual_max <- rbind(ALL_POR_annual_max, z4)
  ALL_POR_annual_min <- rbind(ALL_POR_annual_min, z5)
  ALL_POR_daily_metrics <- rbind(ALL_POR_daily_metrics, z6)
  ALL_POR_monthly_metrics <- rbind(ALL_POR_monthly_metrics, z7)
  rm(z4, z5, z6, z7)
  
  rm(site_no)
}

clock_end <- Sys.time()
clock_runtime <- difftime(clock_end, clock_start, units = c("hours"))
print(clock_runtime)
